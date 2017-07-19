/*
 * text field
 *
 * https://codepen.io/lonekorean/pen/gaLEMR?editors=0010
 */

type state = {
  container_: option Dom.element,
  backdrop_: option Dom.element,
  highlights_: option Dom.element,
  textarea_: option Dom.element,
  linesDiv_: option Dom.element,
  codeLinesDiv_: option Dom.element,
  content: string,
  text_height: int,
  max_text_height: int,
  on_content: string => unit
};

let queries =
  [(0, ".text_field.container"),
    ...[(1, ".text_field .backdrop"),
      ...[(2, ".text_field .highlights"),
        ...[(3, ".text_field textarea"),
          ...[(4, ".lines"),
            ...[(5, ".codelines")]]]]]];

let setReferToState ind opt_ref state => {
  switch (ind, opt_ref) {
  | (0, Some _) => {...state, container_: opt_ref}
  | (1, Some _) => {...state, backdrop_: opt_ref}
  | (2, Some _) => {...state, highlights_: opt_ref}
  | (3, Some _) => {...state, textarea_: opt_ref}
  | (4, Some _) => {...state, linesDiv_: opt_ref}
  | (5, Some _) => {...state, codeLinesDiv_: opt_ref}
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
  | 0 => state.container_
  | 1 => state.backdrop_
  | 2 => state.highlights_
  | 3 => state.textarea_
  | 4 => state.linesDiv_
  | 5 => state.codeLinesDiv_
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

let create_lineno_div st en => {
  let div n => "<div class='lineno'>" ^ (string_of_int n) ^ "</div>";
  let rec h n acc =>
    en < n ? acc : (h (n + 1) (acc ^ (div n)));
  h st ""
};

let setLineNumber height state => {
  let max = state.max_text_height;
  if (height == max) {
    ReasonReact.NoUpdate
  } else if (height < max) {
    let state = {...state, text_height: height, max_text_height: height};
    let (opt_codelines, state) = getReferAndNewState 5 state;
    switch opt_codelines {
    | None => ()
    | Some e => Rutil.remove_last_children (max - height) e
    };
    ReasonReact.Update state
  } else {
    let state = {...state, text_height: height, max_text_height: height};
    let (opt_codelines, state) = getReferAndNewState 5 state;
    switch opt_codelines {
    | None =>
      ()
    | Some e => {
      let txt = create_lineno_div (max + 1) height;
      Rutil.appendHTML e txt
      }
    };
    ReasonReact.Update state;
  }
};

let applyHightlights text => {
  let text = Highlight.main text;
  let x = Rutil.replace_br text;
  x
};

let handleScroll (e : ReactEventRe.UI.t) self => {
  let state = self.ReasonReact.state;
  let (textarea, state) = getReferAndNewState 3 state;
  let (highl, state) = getReferAndNewState 2 state;
  let (codelines, state) = getReferAndNewState 5 state;
  switch (textarea, highl, codelines) {
  | (Some e1, Some e2, Some e3) => {
      let top = Rutil.getScrollTop e1;
      let tops = "margin-top:" ^ (Util.soi (-top)) ^ "px;";
      let left = Rutil.getScrollLeft e1;
      let lefts = "margin-left:" ^ (Util.soi (-left)) ^ "px;";
      Rutil.set_styles e2 (tops ^ lefts);
      Rutil.set_styles e3 tops;
    }
  | _ => ()
  };
  ReasonReact.Update state;
};

let handleInput (e : ReactEventRe.Form.t) {ReasonReact.state: state} => {
  let value = Rutil.value_of_event e;
  let (opt_highlights, state) = getReferAndNewState 2 state;
  switch opt_highlights {
  | None =>
    ()
  | Some e => {
      let text = applyHightlights value;
      Rutil.setInnerHTML e text
    }
  };
  let _ = state.on_content value;
  let state' = {...state, content: value};
  let cnt = Rutil.count_br value;
  setLineNumber cnt state';
};

let addPasteListener elm => {
  let f js_event => {
    Rutil.preventDefault js_event;
    let text = Rutil.getPlainClipboardData js_event;
    Rutil.execCommand "insertText" false text;
    ()
  };
  Rutil.addEventListenterToElement elm "paste" f false;
};

let component = ReasonReact.statefulComponent "";

let make ::onContent _children => {
  ...component,

  initialState: fun _ => {
    container_: None,
    backdrop_: None,
    highlights_: None,
    textarea_: None,
    linesDiv_: None,
    codeLinesDiv_: None,
    content: "if true then 1 else 0",
    text_height: 0,
    max_text_height: 0,
    on_content: onContent
  },

  didMount: fun {ReasonReact.state: state, update} => {
    let _ = onContent state.content;
    let content = state.content;
    let (opt_highlights, state) = getReferAndNewState 2 state;
    switch opt_highlights {
    | Some e =>
      let text = applyHightlights content;
      Rutil.setInnerHTML e text
    | _ => ()
    };
    let cnt = Rutil.count_br content;
    setLineNumber cnt state
  },

  render: fun self => {
    <div className="text_field">
      <div className="lines" ref=(self.update (setRefer 4))>
        <div className="codelines" ref=(self.update (setRefer 5))>
        </div>
      </div>
      <div className="txt_container" ref=(self.update (setRefer 0))>
        <div className="backdrop" ref=(self.update (setRefer 1))>
          <div className="highlights" ref=(self.update (setRefer 2))>
          </div>
        </div>
        <textarea ref=(self.update (setRefer 3)) onScroll=(self.update handleScroll) onInput=(self.update handleInput) wrap="off">
          (Rutil.s2e self.state.content)
        </textarea>
      </div>
    </div>
  }
};
