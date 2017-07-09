/* tama */

type state = {
  isExecuting: bool,
  input: string
};

let component = ReasonReact.statefulComponent "Tama";

let make ::id _children => {
  ...component,

  initialState: fun _ => {
    isExecuting: false,
    input: ""
  },

  render: fun {state} => {
    let _ : state = state;
    <div id=id>
      <TextField />
    </div>
  }
};
