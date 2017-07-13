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
  content: string
};

let queries =
  [(0, ".text_field.container"),
    ...[(1, ".text_field .backdrop"),
      ...[(2, ".text_field .highlights"),
        ...[(3, ".text_field textarea")]]]];

let setRefer ind theRef {ReasonReact.state} => {
  let opt_ref = Js.Null.to_opt theRef;
  let state' =
    switch (ind, opt_ref) {
    | (0, Some _) => {...state, container_: opt_ref}
    | (1, Some _) => {...state, backdrop_: opt_ref}
    | (2, Some _) => {...state, highlights_: opt_ref}
    | (3, Some _) => {...state, textarea_: opt_ref}
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

let applyHightlights text => {
  let text = Highlight.main text;
  let x = Rutil.replace_br text;
  x
};

let handleScroll (e : ReactEventRe.UI.t) self => {
  let textarea = getRefer 3 self;
  let highl = getRefer 2 self;
  switch (textarea, highl) {
  | (Some e1, Some e2) => {
      let top = Rutil.getScrollTop e1;
      /* Rutil.setScrollTop e2 top; */
      let tops = "margin-top:" ^ (Util.soi (-top)) ^ "px";
      Rutil.set_styles e2 tops;
     /* let left = Rutil.getScrollLeft e1;
      Rutil.setScrollLeft e2 left; */
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
  ReasonReact.NoUpdate
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
    content: ""
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
      <div className="lines">
        <div className="codelines">
          (Rutil.s2e "hoge") <br />
          (Rutil.s2e "hoge") <br />
          (Rutil.s2e "hoge") <br />
          (Rutil.s2e "hoge") <br />
          (Rutil.s2e "hoge") <br />
          (Rutil.s2e "hoge")
        </div>
      </div>
      <div className="container text_field" ref=(self.update (setRefer 0))>
        <div className="backdrop" ref=(self.update (setRefer 1))>
          <div className="highlights" ref=(self.update (setRefer 2))>
          </div>
        </div>
        <textarea ref=(self.update (setRefer 3)) onScroll=(self.update handleScroll) onInput=(self.update handleInput)>
          (Rutil.s2e self.state.content)
        </textarea>
      </div>
    </div>
  }
};
