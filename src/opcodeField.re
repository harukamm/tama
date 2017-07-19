/*
 * Opcode field
 *
 */

type state = {
  opcodes_text: string 
};

type retainedProps = {
  sourceText: string
};

let opcodesSetter value {ReasonReact.state: state} => {
  let tokens = Tokenize.main value;
  let tree = Parse.main tokens;
  let tree' = Tamavm_pre.main tree;
  ReasonReact.Update {...state, opcodes_text: value}
};

let component = ReasonReact.statefulComponentWithRetainedProps "opcodeField";

let make ::sourceText _children => {
  ...component,

  initialState: fun _ => {
    opcodes_text: ""
  },

  retainedProps: {
    sourceText: sourceText
  },

  didUpdate: fun {oldSelf, newSelf} => {
    let old_ = oldSelf.retainedProps.sourceText;
    let new_ = newSelf.retainedProps.sourceText;
    if(old_ !== new_) {
      newSelf.update opcodesSetter new_;
      Js.log ("new!: " ^ new_);
    }
  },

  render: fun self => {
    let state : state = self.state;
    <div className="opcodeField">
      <div className="content">
        (Rutil.s2e state.opcodes_text)
      </div>
    </div>
  }
};
