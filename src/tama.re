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
      <div className="title" />
      <div>
        <TextField onContent=(update syncContent)/>
        <div className="op_panel">
          <button className="cmp_btn" onClick=(update compileContent)>
            (Rutil.s2e {js|こんぱいる|js})
          </button>
          <OpcodeField sourceText=(state.source_to_compile)/>
        </div>
      </div>
    </div>
  }
};
