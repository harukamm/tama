/*
let value_of_event event =>
  (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##value;

let name_of_event event =>
  (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##name;

let checked_of_event event =>
  (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##checked;
*/

module Console = {
  type state = { te: string };

  let component = ReasonReact.statefulComponent "ConsoleRe";

  let make ::id _children => {
    ...component,

    initialState: fun _ => { te: "a" },

    render: fun {state} => {
      let x : state = state;
      <textarea rows=10 cols=30 />
    }
  };
};
