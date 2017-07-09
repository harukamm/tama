let value_of_event event =>
  (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##value;

let name_of_event event =>
  (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##name;

let checked_of_event event =>
  (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##checked;

external set_attr : Dom.element => string => string => unit =
  "setAttribute" [@@bs.send];

external out_wid : Dom.element => int =
  "offsetWidth" [@@bs.get];

let set_attribute (k, v) (elm : option Dom.element) =>
  switch elm {
  | None => ()
  | Some e => set_attr e k v
  };

let outer_width (elm : option Dom.element) =>
  switch elm {
  | None => failwith "outer_width"
  | Some e => out_wid e
  };
