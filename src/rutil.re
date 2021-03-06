/* Widely used Reason APIs */

let nulle = ReasonReact.nullElement;

let jbl = Js.Boolean.to_js_boolean;

let tbl b => b == Js.true_ ? true : false;

let jtrue = Js.true_;

let jfalse = Js.false_;

let s2e = ReasonReact.stringToElement;

let keycode = ReactEventRe.Keyboard.which;

let value_of_event event =>
  (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##value;

let name_of_event event =>
  (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##name;

let checked_of_event event =>
  (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##checked;

/* window APIs */

type window;

type ua;

type defaultView_;

type computedStyle_;

type js_event_;

type clipboardData_;

external window : window =
  "" [@@bs.val];

external getUserAgent_ : unit => ua =
  "toLowerCase" [@@bs.val] [@@bs.scope ("window", "navigator", "userAgent")];

external ua_match_ : ua => string => Js.boolean =
  "match" [@@bs.send];

let ua_obj = getUserAgent_ ();

external onload_ : window => (unit => unit) => unit =
  "onload" [@@bs.set];

external addGlobalEventListener_ : window => string => (js_event_ => unit) => Js.boolean => unit =
  "addEventListener" [@@bs.send];

external preventDefault : js_event_ => unit =
  "" [@@bs.send];

external clipboardData_ : js_event_ => clipboardData_ =
  "clipboardData" [@@bs.get];

external getData_ : clipboardData_ => string => string =
  "getData" [@@bs.send];

let addGlobalEventListener typ listener useCapture =>
   addGlobalEventListener_ window typ listener (jbl useCapture);

let set_onload = onload_ window;

let add_onload f => addGlobalEventListener "load" f false;

let getPlainClipboardData e : string =>
  getData_ (clipboardData_ e) "text/plain";

/* Document APIs */

type doc;

external doc : doc =
  "document" [@@bs.val];

external execCommand_ : string => Js.boolean => string => Js.boolean = 
  "execCommand" [@@bs.val] [@@bs.scope "document"];

let execCommand name showDefault value =>
  execCommand_ name (jbl showDefault) value |> tbl;

/* Dom.element APIs */

external dispatchEvent : Dom.element => js_event_ => unit =
  "" [@@bs.send];

type class_lst_;

external getClassList_ : Dom.element => class_lst_ =
  "classList" [@@bs.get];

external addClass_ : class_lst_ => string => unit =
  "add" [@@bs.send];

external removeClass_ : class_lst_ => string => unit =
  "remove" [@@bs.send];

let addClass e c =>
  addClass_ (getClassList_ e) c;

let removeClass e c =>
  removeClass_ (getClassList_ e) c;

external querySelector : string => Dom.element =
  "" [@@bs.val] [@@bs.scope "document"];

external getScrollTop : Dom.element => int =
  "scrollTop" [@@bs.get];

external getScrollRight : Dom.element => int =
  "scrollRight" [@@bs.get];

external getScrollLeft : Dom.element => int =
  "scrollLeft" [@@bs.get];

external setScrollTop : Dom.element => int => unit =
  "scrollTop" [@@bs.set];

external setScrollLeft : Dom.element => int => unit =
  "scrollLeft" [@@bs.set];

external setInnerHTML : Dom.element => string => unit =
  "innerHTML" [@@bs.set];

external getInnerHTML : Dom.element => string =
  "innerHTML" [@@bs.get];

let appendHTML e html =>
  setInnerHTML e ((getInnerHTML e) ^ html);

external textContent : Dom.element => string =
  "" [@@bs.get];

external lastChild_ : Dom.element => Dom.element =
  "lastChild" [@@bs.get];

type childs;

external childNodes_ : Dom.element => childs =
  "" [@@bs.get];

external childrenCount_ : childs => int =
  "length" [@@bs.get];

let getChildrenCount e =>
  childrenCount_ (childNodes_ e);

external addEventListenerToElement_ : Dom.element => string => (js_event_ => unit) => Js.boolean => unit =
  "addEventListener" [@@bs.send];

let addEventListenterToElement elm typ listener showDefault =>
  addEventListenerToElement_ elm typ listener (jbl showDefault);

external createEvent : string => js_event_ =
  "Event" [@@bs.new];

external defaultView_ : defaultView_ =
  "defaultView" [@@bs.scope "document"] [@@bs.val];

external getComputedStyle_ : defaultView_ => Dom.element => computedStyle_ =
  "getComputedStyle" [@@bs.send];

external height : computedStyle_ => string =
  "height" [@@bs.get];

external out_wid : Dom.element => int =
  "offsetWidth" [@@bs.get];

let get_style_height e =>
  height (getComputedStyle_ defaultView_ e);

let outer_width e =>
  out_wid e;

external set_attr : Dom.element => string => string => unit =
  "setAttribute" [@@bs.send];

let set_attribute e (k, v) =>
  set_attr e k v;

let set_styles e styles =>
  set_attribute e ("style", styles);

external elmToString : Dom.element => string =
  "call" [@@bs.val] [@@bs.scope ("Object", "prototype", "toString")];

external removeChild : Dom.element => Dom.element => unit =
  "" [@@bs.send];

let isNullElement e => {
  let str = elmToString e;
  str == "[object Null]"
};

let remove_last_child e => {
  let last = lastChild_ e;
  if (isNullElement last) {
    ()
  } else {
    removeChild e last;
  }
};

let rec remove_last_children cnt e =>
  cnt <= 0 ? () : { remove_last_child e; remove_last_children (cnt - 1) e };

/* other javaScript APIs */

external stringRiteral_ : string => string =
  "stringify" [@@bs.val] [@@bs.scope "JSON"];

external eval_to_int : string => int =
  "eval" [@@bs.val];

external eval_to_string : string => string =
  "eval" [@@bs.val];

let count_br text => {
  let lite = stringRiteral_ text;
  let e = lite ^ ".split('\\n').length";
  eval_to_int e;
};

let replace_br text => {
  let lite = stringRiteral_ text;
  let e = lite ^ ".replace(/\\n/g, '<br>')";
  let e' = eval_to_string e;
  e'
}
