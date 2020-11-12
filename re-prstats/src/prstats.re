open Tablecloth;

type pr = {
  project: string,
  branch: string,
  _id: int,
  author: string,
};

module Project = {
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

  let deccoToStringResult =
      (deccoResult: Decco.result('a)): Result.t('a, string) =>
    deccoResult->Result.mapError(~f=deccoError => deccoError.message);

  let loadResourcesData = (): Result.t(SF.Resources.t, string) =>
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
// run();
let srName = a => {
  switch (a) {
  | SF.SourceRepository.Full(f) => f.name
  | SF.SourceRepository.Name(n) => n
  };
};
let compose = (f, g, x) => g(f(x));
let ok = x => x->Ok;

let addSourceRepositories =
    (
      projectName: string,
      resources: SF.Resources.t,
      topProjects: List.t(Project.t),
    )
    : SF.Resources.t => {
  let newSrs: list(SF.SourceRepository.union) =
    topProjects->List.map(~f=topProject =>
      SF.SourceRepository.Full({
        name: topProject.name,
        connection: None,
        zuul_include: Some([]),
      })
    );

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

  let addSr = (project: SF.Project.t) => {
    project.name == projectName
      ? {
        Js.log4(
          "Will add",
          string_of_int(List.length(topProjects)),
          "distgits to",
          projectName,
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
          let path = "./new-distgits-added.txt"
          ReCli.Python.write_file(newSrsDedupNames, path)
          ->Result.andThen(~f=_ => Js.log("Wrote " ++ path)->Ok)
          ->Result.unwrapUnsafe;
        };
        Js.log(
          "Added (after dedup) " ++ string_of_int(newl - origl) ++ " distgits",
        );
        ret;
      }
      : {
        Js.log2("Skipping", projectName);
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

let dumpResources = (resources: SF.Resources.t) => {
  resources->SF.Resources.encode->ReCli.Python.Yaml.dumps->Ok;
};

Project.loadResourcesData()
->Result.mapError(~f=err => "Unable to load resources: " ++ err)
->Result.andThen(~f=res =>
    Project.getTopProjects()
    ->Result.andThen(~f=prs =>
        addSourceRepositories("Fedora-Distgits", res, prs)->Ok
      )
  )
->Result.andThen(~f=res => res->dumpResources)
->Result.andThen(~f=str => {
    Js.log("Writting into ./fedora-distgits.yaml");
    ReCli.Python.write_file(str, "./fedora-distgits.yaml");
  })
->Result.mapError(~f=err => Js.log("Unable to process due to: " ++ err));
