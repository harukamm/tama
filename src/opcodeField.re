/*
 * Opcode field
 *
 */

type state = {
  opcodes_text: string,
  error_message: ReasonReact.reactElement,
  on_error: (int, int) => unit
};

type retainedProps = {
  sourceText: string
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

let make ::sourceText ::onError _children => {
  ...component,

  initialState: fun _ => {
    opcodes_text: "",
    error_message: Rutil.nulle,
    on_error: onError
  },

  retainedProps: {
    sourceText: sourceText
  },

  didUpdate: fun {oldSelf, newSelf} => {
    let old_ = oldSelf.retainedProps.sourceText;
    let new_ = newSelf.retainedProps.sourceText;
    if(old_ !== new_) {
      newSelf.update opcodesSetter new_;
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
