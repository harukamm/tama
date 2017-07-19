/* tama */

type state = {
  is_executing: bool,
  source_content: string,
  source_to_compile: string,
  mark: (int, int)
};

let syncContent value self => {
  let state = self.ReasonReact.state;
  ReasonReact.SilentUpdate {...state, source_content: value}
};

let onError location self => {
  let state = self.ReasonReact.state;
  switch location {
  | (p1, p2) => ReasonReact.Update {...state, mark: (p1, p2)}
  }
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
    source_to_compile: "",
    mark: (0, 0)
  },

  render: fun {state, update} => {
    <div id=id>
      <div className="title" />
      <div>
        <TextField onContent=(update syncContent) mark=state.mark/>
        <div className="op_panel">
          <button className="cmp_btn" onClick=(update compileContent)>
            (Rutil.s2e {js|こんぱいる|js})
          </button>
          <OpcodeField sourceText=(state.source_to_compile) onError=(update onError) />
        </div>
      </div>
    </div>
  }
};
