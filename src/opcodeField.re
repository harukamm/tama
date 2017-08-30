/*
 * Opcode field
 *
 */

type state = {
  is_ready: bool,
  is_executing: bool,
  out_div: option Dom.element,
  stack_div: option Dom.element,
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

let queries =
  [(0, ".opcode_field .out"),
    ...[(1, ".opcode_field .stack_view")]];

let setReferToState ind opt_ref state => {
  switch (ind, opt_ref) {
  | (0, Some _) => {...state, out_div: opt_ref}
  | (1, Some _) => {...state, stack_div: opt_ref}
  | _ => state
  };
};

let setRefer ind theRef {ReasonReact.state: state} => {
  let opt_ref = Js.Null.to_opt theRef;
  let state' = setReferToState ind opt_ref state;
  ReasonReact.SilentUpdate state';
};

let getReferFromState ind state =>
  switch ind {
  | 0 => state.out_div
  | 1 => state.stack_div
  | _ => None
  };

let getReferFromDoc ind => {
  if (List.length queries <= ind) {
    None
  } else {
    let query = List.assoc ind queries;
    let e = Rutil.querySelector query;
    Rutil.isNullElement e ? None : Some e
  }
};

let getReferAndNewState id state =>
  switch (getReferFromState id state) {
  | None =>
    let opt = getReferFromDoc id;
    (opt, setReferToState id opt state)
  | Some e =>
    (Some e, state)
  };

let outSetter id value state => {
  let (opt_e, state') = getReferAndNewState id state;
  switch opt_e {
  | None => ()
  | Some e => Rutil.setInnerHTML e value
  };
  state'
};

let string_of_process_type (t : Types.process_type) =>
  switch t {
  | Tokenizing => "Tokenize Error"
  | Parsing => "Parsing Error"
  | Prechecking => "Invalid Expression Error"
  | Emitting => "Compile Error"
  | Runtime => "Runtime"
  };

let string_of_loc_info (l : Types.loc_info) =>
  switch l {
  | ((_, l1, c1), (_, l2, c2)) =>
    let (l1', l2') = (l1 + 1, l2 + 1);
    let (c1', c2') = (c1 + 1, c2 + 1);
    if (l1 == l2) {
      "line " ^ (Util.soi l1') ^ ", character " ^
        (Util.soi c1') ^ "-" ^ (Util.soi c2')
    } else {
      "from line " ^ (Util.soi l1') ^ ", character " ^
        (Util.soi c1') ^ " to " ^ (Util.soi l2') ^ ", " ^ (Util.soi c2')
    }
  };

let error_typ_elem (t : Types.process_type) => {
  let typ' = string_of_process_type t;
  <span className="error_typ"> (Rutil.s2e typ') </span>
};

let create_error_elem (t : Types.process_type) msg (l : option Types.loc_info) => {
  let text =
    switch l {
    | None => msg
    | Some x => msg ^ ", " ^ (string_of_loc_info x)
  };
  <span>
    (error_typ_elem t)
    <br />
    (Rutil.s2e (": " ^ text))
  </span>
};

let on_error_location state opt_info =>
  switch opt_info {
  | None => ()
  | Some ((p1, _, _), (p2, _, _)) => state.on_error (p1, p2)
  };

let stepOne () {ReasonReact.state: state} =>
  if(not state.is_ready) {
    ReasonReact.NoUpdate
  } else {
    let result = Tamavm.step_from_reason ();
    switch result {
    | REnd => {
        Js.log "end";
        state.on_end ();
        ReasonReact.NoUpdate
      }
    | RSuccess s =>
        let state' = outSetter 0 s state;
        let stk = Tamavm.stack_content ();
        let state'' = outSetter 1 stk state';
        ReasonReact.Update state''
    | RError typ msg opt_info => {
        let e = create_error_elem typ msg opt_info;
        let () = on_error_location state opt_info;
        ReasonReact.Update {...state, error_message: e}
      }
    }
  };

let quitExecuting () {ReasonReact.state: state} => {
  Tamavm.clear ();
  let s = Tamavm.highlighted_opcode ();
  let state' = outSetter 0 s state;
  ReasonReact.Update {...state', is_executing: false}
};

let startExecuting () {ReasonReact.state: state} => {
  if(not state.is_ready) {
    Js.log "fuga";
    ReasonReact.NoUpdate
  } else {
    Tamavm.init ();
    let s = Tamavm.highlighted_opcode ();
    let state' = outSetter 0 s state;
    let state'' = outSetter 1 "" state';
    ReasonReact.SilentUpdate {...state'', is_executing: true}
  }
};

let opcodesSetter value {ReasonReact.state: state} => {
  let result = Tamavm.compile_from_reason value;
  switch result {
  | RSuccess x =>
    let e =
      <span className="success"> (Rutil.s2e "Success!") </span>;
    state.on_ready true;
    Js.log x;
    Js.log state.out_div;
    let state' = outSetter 0 x state;
    ReasonReact.Update {...state', is_ready: true, error_message: e}
  | RError typ msg opt_info =>
    let e = create_error_elem typ msg opt_info;
    let () = on_error_location state opt_info;
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
    stack_div: None,
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
      <div className="panel1">
        <div className="message">
          state.error_message
        </div>
        <div className="out" ref=(self.update (setRefer 0))>
        </div>
      </div>
      <div className="stack_view" ref=(self.update (setRefer 1))>
      </div>
    </div>
  }
};
