module Main where

import Data.Function
import Control.Monad.Eff
import Debug.Trace

foreign import data Event :: *
foreign import data Handler :: * -> * -> *
foreign import data ComponentDef :: * -> * -> *
foreign import data Element :: *
foreign import data DOM :: !
foreign import data DOMElement :: *

type Component props state = {
  props :: props,
  state :: state,
  children :: [Element]
  }

foreign import _merge
  """
  function _merge(a, b) {
    var c = {};
    var name;
    for (name in a) {
      if (a.hasOwnProperty(name)) {
        c[name] = a[name];
      }
    }
    for (name in b) {
      if (b.hasOwnProperty(name)) {
        c[name] = b[name];
      }
    }
    return c;
  }
  """
  :: forall a b c. Fn2 {|a} {|b} {|c}

foreign import _react
  """
  var _react = require('react')
  """
  :: forall a. a

foreign import _node
  """
  var _node = _react.createElement
  """
  :: forall props state. Fn3 (ComponentDef props state) props [Element] Element

node = runFn3 _node

foreign import _dom
  """
  var _dom = _react.createElement
  """
  :: forall props. Fn3 String props [Element] Element

dom = runFn3 _dom

foreign import text
  """
  function text(text) {
    return text;
  }
  """
  :: String -> Element

foreign import elements
  """
  function elements(elements) {
    return elements;
  }
  """
  :: [Element] -> Element

foreign import _handle
  """
  function _handle(self, func) {
    return function(e) {
      func(self)(e)();
    };
  }
  """
  :: forall props state eff result.
  Fn2 (Component props state) (Component props state -> Event -> Eff eff result) (Event -> Eff eff result)

handle = runFn2 _handle

foreign import _callback
  """
  function _callback(self, func) {
    return function() {
      func(self)();
    };
  }
  """
  :: forall props state eff result.
  Fn2 (Component props state) (Component props state -> Eff eff result) (Eff eff result)

callback = runFn2 _callback

foreign import _setState
  """
  function _setState(self, nextState) {
    return function() {
      self.setState(nextState);
    }
  }
  """
  :: forall props state eff.
  Fn2 (Component props state) state (Eff (eff) Unit)

setState = runFn2 _setState

unwrap (BuildComponentSpec spec) = spec

foreign import _mkComponentSpec
  """
  function _mkComponentSpec() {
    return {};
  }
  """ :: forall props state. Unit -> ComponentSpec props state

foreign import data ComponentSpec :: * -> * -> *
data BuildComponentSpec r = BuildComponentSpec r

instance functorBuildComponentSpec :: Functor BuildComponentSpec where
  (<$>) f (BuildComponentSpec s) = BuildComponentSpec (f s)

instance applyBuildComponentSpec :: Apply BuildComponentSpec where
  (<*>) (BuildComponentSpec f) (BuildComponentSpec s) = BuildComponentSpec (f s)

instance applicativeBuildComponentSpec :: Applicative BuildComponentSpec where
  pure = BuildComponentSpec

foreign import _bindBuildComponentSpecImpl
  """
  function _bindBuildComponentSpec(unwrap, s, f) {
    var spec = unwrap(s);
    console.log(spec);
    return f(spec);
  }
  """
  :: forall a b. Fn3 (BuildComponentSpec a -> a) (BuildComponentSpec a) (a -> BuildComponentSpec b) (BuildComponentSpec b)

bindBuildComponentSpecImpl = runFn3 _bindBuildComponentSpecImpl unwrap

instance bindBuildComponentSpec :: Bind BuildComponentSpec where
  (>>=) = bindBuildComponentSpecImpl

instance monadBuildComponentSpec :: Monad BuildComponentSpec

foreign import _defineComponent
  """
  function _defineComponent(spec) {
    var componentWillMount = spec.componentWillMount;
    spec.componentWillMount = function() {
      this.children = this.props.children;
      if (componentWillMount) {
        componentWillMount();
      }
    };
    console.log(spec);
    return _react.createClass(spec);
  }
  """ :: forall props state. ComponentSpec props state -> ComponentDef props state

defineComponent :: forall props state. (ComponentSpec props state -> BuildComponentSpec (ComponentSpec props state)) -> ComponentDef props state
defineComponent build =
  let built = build (_mkComponentSpec unit) in
  _defineComponent (unwrap built)


foreign import _render
  """
  function _render(pure, render, spec) {
    spec = _merge(spec, {
      render: function() {
        return render(this);
      }
    });
    return pure(spec);
  }
  """ ::
  forall props state. Fn3
    (ComponentSpec props state -> BuildComponentSpec (ComponentSpec props state))
    (Component props state -> Element)
    (ComponentSpec props state)
    (BuildComponentSpec (ComponentSpec props state))


render = runFn3 _render BuildComponentSpec


foreign import _componentWillMount
  """
  function _componentWillMount(pure, componentWillMount, spec) {
    spec = _merge(spec, {
      componentWillMount: function() {
        return componentWillMount(this)();
      }
    });
    return pure(spec);
  }
  """ ::
  forall props state eff result. Fn3
    (ComponentSpec props state -> BuildComponentSpec (ComponentSpec props state))
    (Component props state -> Eff (eff) result)
    (ComponentSpec props state)
    (BuildComponentSpec (ComponentSpec props state))

componentWillMount = runFn3 _componentWillMount BuildComponentSpec


foreign import _componentDidMount
  """
  function _componentDidMount(pure, componentDidMount, spec) {
    spec = _merge(spec, {
      componentDidMount: function() {
        return componentDidMount(this)();
      }
    });
    return pure(spec);
  }
  """ ::
  forall props state eff result. Fn3
    (ComponentSpec props state -> BuildComponentSpec (ComponentSpec props state))
    (Component props state -> Eff (eff) result)
    (ComponentSpec props state)
    (BuildComponentSpec (ComponentSpec props state))

componentDidMount = runFn3 _componentDidMount BuildComponentSpec

foreign import _componentWillUnmount
  """
  function _componentWillUnmount(pure, componentWillUnmount, spec) {
    spec = _merge(spec, {
      componentWillUnmount: function() {
        return componentWillUnmount(this)();
      }
    });
    return pure(spec);
  }
  """ ::
  forall props state eff result. Fn3
    (ComponentSpec props state -> BuildComponentSpec (ComponentSpec props state))
    (Component props state -> Eff (eff) result)
    (ComponentSpec props state)
    (BuildComponentSpec (ComponentSpec props state))

componentWillUnmount = runFn3 _componentWillUnmount BuildComponentSpec

foreign import _componentDidUpdate
  """
  function _componentDidUpdate(pure, componentDidUpdate, spec) {
    spec = _merge(spec, {
        componentDidUpdate: function(prevProps, prevState) {
        return componentDidUpdate(this)(prevProps)(prevState)();
      }
    });
    return pure(spec);
  }
  """ ::
  forall props state eff result. Fn3
    (ComponentSpec props state -> BuildComponentSpec (ComponentSpec props state))
    (Component props state -> props -> state -> Eff (eff) result)
    (ComponentSpec props state)
    (BuildComponentSpec (ComponentSpec props state))

componentDidUpdate = runFn3 _componentDidUpdate BuildComponentSpec

foreign import _componentWillReceiveProps
  """
  function _componentWillReceiveProps(pure, componentWillReceiveProps, spec) {
    spec = _merge(spec, {
      componentWillReceiveProps: function(nextProps) {
        return componentWillReceiveProps(this)(nextProps)(nextState)();
      }
    });
    return pure(spec);
  }
  """ ::
  forall props state eff result. Fn3
    (ComponentSpec props state -> BuildComponentSpec (ComponentSpec props state))
    (Component props state -> props -> state -> Eff (eff) result)
    (ComponentSpec props state)
    (BuildComponentSpec (ComponentSpec props state))

componentWillReceiveProps = runFn3 _componentWillReceiveProps BuildComponentSpec

foreign import _initialState
  """
  function _initialState(pure, initialState, spec) {
    spec = _merge(spec, {
      getInitialState: function() {
        return initialState;
      }
    });
    return pure(spec);
  }
  """ ::
  forall props state eff result. Fn3
    (ComponentSpec props state -> BuildComponentSpec (ComponentSpec props state))
    state
    (ComponentSpec props state)
    (BuildComponentSpec (ComponentSpec props state))

initialState = runFn3 _initialState BuildComponentSpec

foreign import renderToString
  """
  var renderToString = _react.renderToString
  """
  :: Element -> String

foreign import getElementById
  """
  var getElementById = document.getElementById.bind(document);
  """
  :: String -> DOMElement

foreign import _renderToDOM
  """
  var _renderToDOM = _react.render
  """
  :: forall eff. Fn2 Element DOMElement (Eff (dom :: DOM | eff) Unit)

renderToDOM = runFn2 _renderToDOM

foreign import data Timer :: !

foreign import _setInterval
  """
  function _setInterval(action, ms) {
    return function() {
      setInterval(action, ms)
    };
  }
  """
  :: forall eff eff2 result. Fn2 (Eff (eff) result) Number (Eff (timer :: Timer | eff2) Unit)

setTimer = runFn2 _setInterval

ticker = defineComponent do

  initialState {tick: 0}

  render \self ->
    dom "div" {onClick: handle self onClick} [
      text (show self.state.tick),
      elements self.children
      ]

  componentDidMount \self -> do
    setTimer (callback self tick) self.props.interval

    where

  onClick self ev = do
    trace self.props.name

  tick self = do
    setState self {tick: self.state.tick + 1}


main = do
  let app = dom "body" {} [
              node ticker {name: "Hello", interval: 1000} [
                dom "div" {} [text "hello"]
              ]
            ]
  let root = getElementById "main"
  renderToDOM app root
