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
  text_height: int
};

let queries =
  [(0, ".text_field.container"),
    ...[(1, ".text_field .backdrop"),
      ...[(2, ".text_field .highlights"),
        ...[(3, ".text_field textarea"),
          ...[(4, ".lines"),
            ...[(5, ".codelines")]]]]]];

let setRefer ind theRef {ReasonReact.state} => {
  let opt_ref = Js.Null.to_opt theRef;
  let state' =
    switch (ind, opt_ref) {
    | (0, Some _) => {...state, container_: opt_ref}
    | (1, Some _) => {...state, backdrop_: opt_ref}
    | (2, Some _) => {...state, highlights_: opt_ref}
    | (3, Some _) => {...state, textarea_: opt_ref}
    | (4, Some _) => {...state, linesDiv_: opt_ref}
    | (5, Some _) => {...state, codeLinesDiv_: opt_ref}
    | _ => state
    };
  ReasonReact.SilentUpdate state'
};

let getRefer ind {ReasonReact.state} => {
  let get_opt_ref index =>
    switch index {
    | 0 => state.container_
    | 1 => state.backdrop_
    | 2 => state.highlights_
    | 3 => state.textarea_
    | 4 => state.linesDiv_
    | 5 => state.codeLinesDiv_
    | _ => None
    };
  let opt_ref = get_opt_ref ind;
  switch opt_ref {
  | None =>
    if (List.length queries <= ind) {
      None
    } else {
      let query = List.assoc ind queries;
      let e = Rutil.querySelector query;
      Rutil.isNullElement e ? None : Some e
    }
  | Some e => Some e
  }
};

let setTextarea (theRef : Js.null Dom.element) self => {
  let opt_ref : option Dom.element = Js.Null.to_opt theRef;
  setRefer 3 theRef self
};

let create_lineno_div ht => {
  let div n => "<div class='lineno'>" ^ (string_of_int n) ^ "</div>";
  let rec h n acc =>
    ht < n ? acc : (h (n + 1) (acc ^ (div n)));
  h 1 ""
};

let setLineNumber height self => {
  let opt_codelines = getRefer 5 self;
  switch opt_codelines {
  | None =>
    ()
  | Some e => {
      let txt = create_lineno_div height;
      Rutil.setInnerHTML e txt;
    }
  };
  ReasonReact.NoUpdate
};

let applyHightlights text => {
  let text = Highlight.main text;
  let x = Rutil.replace_br text;
  x
};

let handleScroll (e : ReactEventRe.UI.t) self => {
  let textarea = getRefer 3 self;
  let highl = getRefer 2 self;
  let codelines = getRefer 5 self;
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
  ReasonReact.NoUpdate 
};

let handleInput (e : ReactEventRe.Form.t) self => {
  let value = Rutil.value_of_event e;
  let opt_highlights = getRefer 2 self;
  switch opt_highlights {
  | None =>
    ()
  | Some e => {
      let text = applyHightlights value;
      Rutil.setInnerHTML e text
    }
  };
  let cnt = Rutil.count_br value;
  Js.log cnt;
  setLineNumber cnt self;
  ReasonReact.Update {...self.state, content: value, text_height: cnt}
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

let make _children => {
  ...component,

  initialState: fun _ => {
    container_: None,
    backdrop_: None,
    highlights_: None,
    textarea_: None,
    linesDiv_: None,
    codeLinesDiv_: None,
    content: "",
    text_height: 1
  },

  didMount: fun self => {
    let opt_highlights = getRefer 2 self;
    switch opt_highlights {
    | Some e =>
      let text = applyHightlights self.state.content;
      Rutil.setInnerHTML e text
    | _ => ()
    };
    ReasonReact.NoUpdate
  },

  render: fun self => {
    <div>
      <div className="lines" ref=(self.update (setRefer 4))>
        <div className="codelines" ref=(self.update (setRefer 5))>
        </div>
      </div>
      <div className="container text_field" ref=(self.update (setRefer 0))>
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
