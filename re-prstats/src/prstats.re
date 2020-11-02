open Tablecloth;

type pr = {
  project: string,
  branch: string,
  _id: int,
  author: string,
};

type project = {
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

let loadData = (): Result.t(List.t(pr), string) => {
  loadJsonData()->Result.andThen(~f=decode);
};

let loadYAMLtoJSON = (content: string): Result.t(Js.Json.t, string) => {
  let rec s = yaml =>
    switch (yaml) {
    | `Bool(value) => Js.Json.boolean(value)
    | `Float(number) => Js.Json.number(number)
    | `String(str) => Js.Json.string(str)
    | `Array(arr) => List.map(~f=s, arr) |> Array.fromList |> Js.Json.array
    | `Object(list) =>
      Js.Dict.fromList(
        List.map(~f=((key, value)) => (key, s(value)), list),
      )
      |> Js.Json.object_
    | `Null => Js.Json.null
    };
  let parse = (content: string) => {
    Yaml.parse(content)->s;
  };
  ReCli.Python.catchToResult(parse, content);
};

let loadResourcesData = (): Result.t(SF.Resources.top, string) => {
  "/home/user/git/pagure.io/fedora-project-config/resources/fedora-distgits.yaml"
  ->ReCli.Python.read_file
  ->Result.andThen(~f=loadYAMLtoJSON)
  ->Result.andThen(~f=json => Ok(SF.Resources.parse(json)));
};

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
    (data: List.t(pr)): Result.t(List.t(project), string) => {
  Ok(
    data
    ->getProjectsList
    ->{
        List.fold(~initial=[]: List.t(project), ~f=(acc, elm) =>
          List.append(
            [{name: elm, totalPrs: getPRCountByProject(data, elm)}],
            acc,
          )
        );
      },
  );
};

let filterProjectByPRCount = (data: List.t(project), prcount: int) => {
  data->List.filter(~f=p => p.totalPrs >= prcount);
};

let run = () => {
  loadData()
  ->Result.andThen(~f=getPRCountByProjects)
  ->Result.unwrap(~default=[])
  ->filterProjectByPRCount(2)
  ->{
      (data: List.t(project)) => {
        Js.log("Total: " ++ (List.length(data) |> Int.toString));
        data;
      };
    }
  ->List.sort(~compare=(e1, e2) => String.compare(e1.name, e2.name))
  ->List.map(~f=Js.log);
};

// run();
loadResourcesData()
->Result.unwrap(
    ~default={
      resources: {
        projects: Some([]),
        connections: Some([]),
        tenants: Some([]),
        repos: Some([]),
      },
    },
  )
->{
    (top: SF.Resources.top) => {
      Js.log(top);
    };
  };
