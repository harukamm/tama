/*
 * Opcode field
 *
 */

type state = {
  is_executing: bool,
  opcodes_text: string,
  error_message: ReasonReact.reactElement,
  on_error: (int, int) => unit
};

type retainedProps = {
  step: int,
  sourceText: string,
  isExecuting: bool
};

let stepOne () {ReasonReact.state: state} => {
  ReasonReact.NoUpdate
};

let stopExecuting () {ReasonReact.state: state} => {
  ReasonReact.NoUpdate
};

let startExecuting () {ReasonReact.state: state} => {
  ReasonReact.NoUpdate
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
    ReasonReact.Update {...state, opcodes_text: x, error_message: e}
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
    ReasonReact.Update {...state, error_message: e}
  }
};

let component = ReasonReact.statefulComponentWithRetainedProps "opcodeField";

let make ::isExecuting ::sourceText ::step ::onError _children => {
  ...component,

  initialState: fun _ => {
    is_executing: isExecuting,
    opcodes_text: "",
    error_message: Rutil.nulle,
    on_error: onError
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
      newSelf.update stopExecuting ();
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
      <div className="out">
        (Rutil.s2e state.opcodes_text)
      </div>
    </div>
  }
};
