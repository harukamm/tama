/*
 * Opcode field
 *
 */

type state = {
  is_ready: bool,
  is_executing: bool,
  out_div: option Dom.element,
  error_message: ReasonReact.reactElement,
  on_error: (int, int) => unit,
  on_ready: bool => unit,
  on_end: unit => unit
};

type retainedProps = {
  step: int,
  sourceText: string,
  isExecuting: bool
};

let setOutRef theRef {ReasonReact.state: state} => {
  let opt_ref = Js.Null.to_opt theRef;
  let opt_ref' =
    switch opt_ref {
    | None => {
        let query = ".opcode_field .out";
        let e = Rutil.querySelector query;
        Rutil.isNullElement e ? None : Some e
      };
    | Some _ => opt_ref
    };
  ReasonReact.SilentUpdate {...state, out_div: opt_ref'}
};

let outSetter value opt_e =>
  switch opt_e {
  | None => ()
  | Some e => Rutil.setInnerHTML e value
  };

let stepOne () {ReasonReact.state: state} => {
  if(not state.is_ready) {
    ReasonReact.NoUpdate
  } else {
    Tamavm.step_exe_main ();
    let s = Tamavm.highlighted_opcode ();
    outSetter s state.out_div; 
    ReasonReact.NoUpdate
  }
};

let quitExecuting () {ReasonReact.state: state} => {
  Tamavm.clear ();
  let s = Tamavm.highlighted_opcode ();
  outSetter s state.out_div;
  ReasonReact.Update {...state, is_executing: false}
};

let startExecuting () {ReasonReact.state: state} => {
  if(not state.is_ready) {
    Js.log "fuga";
    ReasonReact.NoUpdate
  } else {
    Tamavm.init ();
    let s = Tamavm.highlighted_opcode ();
    outSetter s state.out_div;
    ReasonReact.SilentUpdate {...state, is_executing: true}
  }
};

let error_typ_text (t : Types.error_typ) =>
  switch t {
  | Tokenizing => "Tokenize Error"
  | Parsing => "Parsing Error"
  | Prechecking => "Invalid Expression Error"
  | Emitting => "Compile Error"
  };

let opcodesSetter value {ReasonReact.state: state} => {
  let result = Tamavm.from_reason value;
  switch result {
  | RSuccess x =>
    let e =
      <span className="success"> (Rutil.s2e "Success!") </span>;
    state.on_ready true;
    outSetter x state.out_div;
    ReasonReact.Update {...state, is_ready: true, error_message: e}
  | RError typ msg opt_info =>
    let typ' = error_typ_text typ;
    let e =
      <span>
        (<span className="error_typ"> (Rutil.s2e typ') </span>)
        (Rutil.s2e (": " ^ msg))
      </span>;
    switch opt_info {
    | None => ()
    | Some ((p1, _, _), (p2, _, _)) => state.on_error (p1, p2)
    };
    state.on_ready false;
    ReasonReact.Update {...state, is_ready: false, error_message: e}
  }
};

let component = ReasonReact.statefulComponentWithRetainedProps "opcodeField";

let make ::isExecuting ::sourceText ::step ::onError ::onReady ::onEnd _children => {
  ...component,

  initialState: fun _ => {
    is_ready: false,
    is_executing: isExecuting,
    out_div: None,
    error_message: Rutil.nulle,
    on_error: onError,
    on_ready: onReady,
    on_end: onEnd
  },

  retainedProps: {
    step: step,
    sourceText: sourceText,
    isExecuting: isExecuting
  },

  didUpdate: fun {oldSelf, newSelf} => {
    let old_ = oldSelf.retainedProps;
    let new_ = newSelf.retainedProps;
    if(old_.sourceText !== new_.sourceText) {
      newSelf.update opcodesSetter new_.sourceText;
    };
    if(old_.step !== new_.step && new_.isExecuting) {
      newSelf.update stepOne ();
    };
    if(old_.isExecuting && not new_.isExecuting) {
      newSelf.update quitExecuting ();
    };
    if(not old_.isExecuting && new_.isExecuting) {
      newSelf.update startExecuting ();
    }
  },

  render: fun self => {
    let state : state = self.state;
    <div className="opcode_field">
      <div className="message">
        state.error_message
      </div>
      <div className="out" ref=(self.update setOutRef)>
      </div>
    </div>
  }
};
