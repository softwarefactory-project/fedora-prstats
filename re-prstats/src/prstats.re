open Tablecloth;

let compose = (f, g, x) => g(f(x));
let ok = x => x->Ok;

type pr = {
  project: string,
  branch: string,
  _id: int,
  author: string,
};

let deccoToStringResult =
    (deccoResult: Decco.result('a)): Result.t('a, string) =>
  deccoResult->Result.mapError(~f=deccoError => deccoError.message);

module Pagure = {
  [@decco]
  type access_users_t = {owner: list(string)};

  [@decco]
  type project_t = {
    fullname: string,
    access_users: access_users_t,
  };

  [@decco]
  type group_t = {
    name: string,
    projects: list(project_t),
  };

  let get_group =
      (baseurl: string, name: string)
      : Js.Promise.t(Result.t(group_t, string)) => {
    let url = baseurl ++ name ++ "?projects=1";
    Js.log("Fetching " ++ url ++ "...");
    Js.Promise.(
      Axios.get(url)
      |> then_(resp => resp##data |> resolve)
      |> then_(json => json->group_t_decode->deccoToStringResult |> resolve)
    );
  };

  // curry to src.fedoraproject.org
  let s_fp_get_group =
    get_group("https://src.fedoraproject.org/api/0/group/");

  let getGroupProjectList =
      (name: string): Js.Promise.t(Result.t(list(project_t), string)) => {
    Js.Promise.(
      s_fp_get_group(name)
      |> then_(result =>
           result |> Result.andThen(~f=group => group.projects->Ok) |> resolve
         )
    );
  };

  let checkDeadPackage =
      (baseurl: string, name: string): Js.Promise.t((string, bool)) => {
    let url = baseurl ++ name ++ "/tree/master/f/dead.package";
    Js.log("Checking dead package for " ++ name ++ " ...");
    Js.Promise.(
      Axios.get(url)
      |> then_(_resp =>
           {
             Js.log2("Dead package: ", name);
             (name, true);
           }
           |> resolve
         )
      |> catch(_err => (name, false) |> resolve)
    );
  };

  // curry to src.fedoraproject.org
  let s_fp_checkDeadPackage =
    checkDeadPackage("https://src.fedoraproject.org/api/0/");

  let isOrphan = (project: project_t): bool => {
    project.access_users.owner
    |> List.filter(~f=user => user == "orphan")
    |> List.length > 0;
  };

  let fromProjectListToNameList = (projects: list(project_t)): list(string) => {
    // We will skip orphaned project
    projects
    ->List.filter(~f=p => !p->isOrphan)
    ->List.map(~f=project => project.fullname);
  };

  let removeDeadPackages =
      (projects: list(string)): Js.Promise.t(list(string)) => {
    let promises = projects->List.map(~f=s_fp_checkDeadPackage);
    let reduceToNotDeadProject = (e: list((string, bool))): list(string) => {
      e
      ->List.filter(~f=((_, isDead)) => !isDead)
      ->List.map(~f=((name, _)) => name);
    };

    Js.Promise.(
      all(promises |> Array.fromList)
      |> then_(res => res->Array.to_list->reduceToNotDeadProject |> resolve)
    );
  };

  let s_fp_getGroupPackagesWODeadOne =
      (name: string)
      : Js.Promise.t(Result.t(Js.Promise.t(list(string)), string)) => {
    Js.Promise.(
      getGroupProjectList(name)
      |> then_(result =>
           result->Result.andThen(~f=l =>
             l->fromProjectListToNameList->removeDeadPackages->Ok
           )
           |> resolve
         )
    );
  };
};

module Resources = {
  let loadData = (): Result.t(SF.Resources.t, string) =>
    "~/git/pagure.io/fedora-project-config/resources/fedora-distgits.yaml"
    ->{
        path => {
          Js.log2("Reading", path);
          path;
        };
      }
    ->ReCli.Python.Yaml.load
    ->Result.andThen(~f=json =>
        json->SF.Resources.decode->deccoToStringResult
      );

  let dumpResources = (resources: SF.Resources.t) => {
    resources->SF.Resources.encode->ReCli.Python.Yaml.dumps->Ok;
  };

  let srName = a => {
    switch (a) {
    | SF.SourceRepository.Full(f) => f.name
    | SF.SourceRepository.Name(n) => n
    };
  };

  let rec removeDup =
          (
            tp: list(SF.SourceRepository.union),
            projectsr: list(SF.SourceRepository.union),
          )
          : list(SF.SourceRepository.union) => {
    switch (tp) {
    | [] => []
    | [hd, ...tl] =>
      Belt.List.has(projectsr, hd, (a, b) => srName(a) == srName(b))
        ? removeDup(tl, projectsr)
        : Belt.List.concat([hd], removeDup(tl, projectsr))
    };
  };

  let addSourceRepositories =
      (
        sfProjectName: string,
        resources: SF.Resources.t,
        newProjects: List.t(string),
      )
      : SF.Resources.t => {
    let newSrs: list(SF.SourceRepository.union) =
      newProjects->List.map(~f=newProject =>
        SF.SourceRepository.Full({
          name: newProject,
          connection: None,
          zuul_include: Some([]),
        })
      );

    let addSr = (project: SF.Project.t) => {
      project.name == sfProjectName
        ? {
          Js.log4(
            "Will add",
            string_of_int(List.length(newProjects)),
            "distgits to",
            sfProjectName,
          );
          let newSrsDedup = removeDup(newSrs, project.sourceRepositories);
          let ret = {
            ...project,
            sourceRepositories:
              List.append(newSrsDedup, project.sourceRepositories)
              ->List.sort(~compare=(a, b) =>
                  String.compare(a->srName, b->srName)
                ),
          };
          let origl = project.sourceRepositories->Belt.List.length;
          let newl = ret.sourceRepositories->Belt.List.length;
          let newSrsDedupNames =
            newSrsDedup->Belt.List.map(e => srName(e))
            |> List.fold(~initial="", ~f=(output, name) =>
                 output ++ name ++ "\n"
               );
          {
            let path = "./new-distgits-added.txt";
            ReCli.Python.write_file(newSrsDedupNames, path)
            ->Result.andThen(~f=_ => Js.log("Wrote " ++ path)->Ok)
            ->Result.unwrapUnsafe;
          };
          Js.log(
            "Added (after dedup) "
            ++ string_of_int(newl - origl)
            ++ " distgits",
          );
          ret;
        }
        : {
          Js.log2("Skipping", sfProjectName);
          project;
        };
    };
    {
      ...resources,
      projects:
        resources.projects
        ->Option.andThen(~f=projects => projects->List.map(~f=addSr)->Some),
    };
  };
};

module DataGrepper = {
  type t = {
    name: string,
    totalPrs: int,
  };

  let loadJsonData = () => {
    let path = "../../datagrepper/prs.json";
    ReCli.Python.Json.load(path);
  };

  let decodePRObject = (json: Js.Json.t): pr => {
    Json.Decode.{
      project: json |> field("project", string),
      branch: json |> field("branch", string),
      _id: json |> field("_id", int),
      author: json |> field("author", string),
    };
  };

  let decode = (json: ReCli.Python.Json.t): Result.t('a, string) => {
    switch (Js.Json.decodeArray(json)) {
    | Some(json) => Ok(json->Array.to_list->List.map(~f=decodePRObject))
    | None => Error("Unable to decode the Json file, expecting an array.")
    };
  };

  let loadData = (): Result.t(List.t(pr), string) =>
    loadJsonData()->Result.andThen(~f=decode);

  let getProjectsList = (data: List.t(pr)): List.t(string) => {
    data
    // Keep unique project names
    ->List.fold(~initial=[], ~f=(acc, elm) => {
        List.includes(acc, elm.project, ~equal=(==))
          ? acc : List.append(acc, [elm.project])
      })
    // Keep project with name starting with "rpms/"
    ->List.filter(~f=Tablecloth.String.startsWith(~prefix="rpms/"));
  };

  let getProjectsCount = (data: List.t(pr)): Result.t(int, string) => {
    Ok(getProjectsList(data)->List.length);
  };

  let getPRCountByProject = (data: List.t(pr), project: string) => {
    List.fold(data, ~initial=0, ~f=(acc, elm) => {
      elm.project == project ? acc + 1 : acc
    });
  };

  let getPRCountByProjects =
      (data: List.t(pr)): Result.t(List.t(t), string) =>
    data
    ->getProjectsList
    ->List.fold(~initial=[]: List.t(t), ~f=(acc, elm) =>
        List.append(
          [{name: elm, totalPrs: getPRCountByProject(data, elm)}],
          acc,
        )
      )
    ->Ok;

  let filterProjectByPRCount = (data: List.t(t), prcount: int) => {
    data->List.filter(~f=p => p.totalPrs >= prcount);
  };

  let getTopProjects = () => {
    loadData()
    ->Result.andThen(~f=getPRCountByProjects)
    ->Result.andThen(~f=topProjects =>
        filterProjectByPRCount(topProjects, 2)
        ->List.sort(~compare=(e1, e2) => String.compare(e1.name, e2.name))
        ->Ok
      );
  };
};

let fromDGtoNewResources = (outputpath: string, projectname: string) => {
  Resources.loadData()
  ->Result.mapError(~f=err => "Unable to load resources: " ++ err)
  ->Result.andThen(~f=res =>
      DataGrepper.getTopProjects()
      ->Result.andThen(~f=prs =>
          Resources.addSourceRepositories(
            projectname,
            res,
            prs->List.map(~f=p => p.name),
          )
          ->Ok
        )
    )
  ->Result.andThen(~f=res => res->Resources.dumpResources)
  ->Result.andThen(~f=str => {
      Js.log("Writting into " ++ outputpath);
      ReCli.Python.write_file(str, outputpath);
    })
  ->Result.mapError(~f=err => Js.log("Unable to process due to: " ++ err));
};

let fromPagureGrouptoNewResources =
    (outputpath: string, projectname: string, groupname: string) => {
  let process = (res: SF.Resources.t) => {
    Js.Promise.(
      Pagure.s_fp_getGroupPackagesWODeadOne(groupname)
      |> then_(result =>
           result->Result.andThen(~f=p =>
             Ok(
               p
               |> then_(newProjects =>
                    Resources.addSourceRepositories(
                      projectname,
                      res,
                      newProjects,
                    )
                    ->Ok
                    |> Result.andThen(~f=res => res->Resources.dumpResources)
                    |> Result.andThen(~f=str => {
                         Js.log("Writting into " ++ outputpath);
                         ReCli.Python.write_file(str, outputpath);
                       })
                    |> resolve
                  ),
             )
           )
           |> resolve
         )
      |> catch(err => {
           Js.log(err);
           "Error occured"->Error |> resolve;
         })
    );
  };
  Resources.loadData()
  ->Result.mapError(~f=err => "Unable to load resources: " ++ err)
  ->Result.andThen(~f=res => res->process->Ok);
};

// fromDGtoNewResources("./fedora-distgits.yaml", "Fedora-Distgits");
fromPagureGrouptoNewResources(
  "./fedora-distgits.yaml",
  "Fedora-Distgits",
  "openstack-sig",
);
