/* tama */

type state = {
  is_executing: bool,
  source_content: string,
  source_to_compile: string
};

let syncContent value self => {
  let state = self.ReasonReact.state;
  ReasonReact.SilentUpdate {...state, source_content: value}
};

let compileContent _ self => {
  let state = self.ReasonReact.state;
  let c = state.source_content;
  Js.log "changed source";
  ReasonReact.Update {...state, source_to_compile: c}
};

let component = ReasonReact.statefulComponent "Tama";

let make ::id _children => {
  ...component,

  initialState: fun _ => {
    is_executing: false,
    source_content: "",
    source_to_compile: ""
  },

  render: fun {state, update} => {
    <div id=id>
      <TextField onContent=(update syncContent)/>
      <div className="op_panel">
        <button onClick=(update compileContent)> (Rutil.s2e "compile") </button>
        <OpcodeField sourceText=(state.source_to_compile)/>
      </div>
    </div>
  }
};
