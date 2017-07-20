/* tama */

type state = {
  ready_to_execute: bool,
  is_executing: bool,
  source_content: string,
  source_to_compile: string,
  mark: (int, int),
  step: int
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

let stepOne _ {ReasonReact.state: state} => {
  ReasonReact.Update {...state, step: state.step + 1}
};

let compileContent _ self => {
  let state = self.ReasonReact.state;
  if(state.is_executing) {
    ReasonReact.Update state
  } else {
    let c = state.source_content;
    Js.log "changed source";
    ReasonReact.Update {...state, source_to_compile: c}
  }
};

let onReadyToExecute b {ReasonReact.state: state} => {
  Js.log b;
  ReasonReact.Update {...state, ready_to_execute: b}
};

let onStart _ {ReasonReact.state: state} => {
  if(state.ready_to_execute) {
    ReasonReact.Update {...state, is_executing: true}
  } else {
    ReasonReact.NoUpdate
  }
};

let onQuit _ {ReasonReact.state: state} => {
  ReasonReact.Update {...state, is_executing: false}
};

let component = ReasonReact.statefulComponent "Tama";

let make ::id _children => {
  ...component,

  initialState: fun _ => {
    ready_to_execute: false,
    is_executing: false,
    source_content: "",
    source_to_compile: "",
    mark: (0, 0),
    step: -1
  },

  render: fun {state, update} => {
    <div id=id>
      <div className="title" />
      <div className="tama">
        <TextField onContent=(update syncContent) mark=state.mark lock=(state.is_executing)/>
        <div className="op_panel">
            <button className="cmp_btn" onClick=(update compileContent)
              disabled=(Rutil.jbl state.is_executing)>
              (Rutil.s2e {js|こんぱいる|js})
            </button>
            <div className="action">
              <button className="start_btn"
                disabled=(Rutil.jbl (not state.ready_to_execute || state.is_executing))
                onClick=(update onStart)>
                (Rutil.s2e {js|やる|js})
              </button>
              <button className="quit_btn" disabled=(Rutil.jbl (not state.is_executing))
                onClick=(update onQuit)>
                (Rutil.s2e {js|やめる|js})
              </button>
              <button className="step_btn" onClick=(update stepOne) disabled=(Rutil.jbl (not state.is_executing))>
                (Rutil.s2e {js|つぎ|js})
              </button>
            </div>
          <OpcodeField isExecuting=(state.is_executing) sourceText=(state.source_to_compile)
            onError=(update onError) onEnd=(update onQuit) step=state.step onReady=(update onReadyToExecute)/>
        </div>
      </div>
    </div>
  }
};
