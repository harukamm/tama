/* text field */

type state = {
  isExecuting: bool,
  input: string,
  textarea_ref: option Dom.element
};

let setTextRef (theRef : Js.null Dom.element) {ReasonReact.state} => {
  let opt_ref : option Dom.element = Js.Null.to_opt theRef;
  Rutil.set_attribute ("wrap", "off") opt_ref;
  let i = Rutil.outer_width opt_ref;
  Js.log i;
  Js.log (string_of_int i);
  ReasonReact.SilentUpdate {...state, textarea_ref: opt_ref}
};

let component = ReasonReact.statefulComponent "TextField";

let make _children => {
  ...component,

  initialState: fun _ => {
    isExecuting: false,
    input: "",
    textarea_ref: None
  },

  render: fun self => {
    <textarea id="text_field" ref=(self.update setTextRef) />
  }
};
