module Test.Node.EventEmitter where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Uncurried (mkEffectFn1, mkEffectFn2, mkEffectFn3, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5)
import Node.EventEmitter (EventEmitter, EventHandle(..), on, on_, once, once_, prependListener, prependListener_, prependOnceListener, prependOnceListener_, unsafeEmitFn1, unsafeEmitFn2, unsafeEmitFn3, unsafeEmitFn4)
import Node.EventEmitter as EventEmitter
import Node.EventEmitter.UtilTypes (EventHandle0, EventHandle1, EventHandle2, EventHandle3)
import Node.Symbol (JsSymbol)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

fooH :: EventHandle1 EventEmitter String
fooH = EventHandle "foo" mkEffectFn1

blankH :: EventHandle0 EventEmitter
blankH = EventHandle "blank" identity

barH :: EventHandle1 EventEmitter String
barH = EventHandle "bar" mkEffectFn1

pairH :: EventHandle2 EventEmitter String String
pairH = EventHandle "pair" mkEffectFn2

tripleH :: EventHandle3 EventEmitter String String String
tripleH = EventHandle "triple" mkEffectFn3

nameOf :: Either JsSymbol String -> String
nameOf = case _ of
  Left _ -> "<symbol>"
  Right name -> name

spec :: Spec Unit
spec = describe "event-emitter" do
  it "`new` does not throw" do
    liftEffect $ void $ EventEmitter.new
  it "`emit` does not throw" do
    liftEffect do
      ee <- EventEmitter.new
      void $ runEffectFn2 unsafeEmitFn1 ee "foo"
      void $ runEffectFn3 unsafeEmitFn2 ee "foo" "baz"
      void $ runEffectFn2 unsafeEmitFn1 ee "foo"
      void $ runEffectFn3 unsafeEmitFn2 ee "foo" "bar"
      void $ runEffectFn4 unsafeEmitFn3 ee "foo" "bar" "baz"
      void $ runEffectFn5 unsafeEmitFn4 ee "foo" "bar" "baz" "qux"
  describe "standard functions" do
    let
      fns =
        [ "on_" /\ on_
        , "once_" /\ once_
        , "prependListener_" /\ prependListener_
        , "prependOnceListener_" /\ prependOnceListener_
        ]
    for_ fns \(fnName /\ fn) -> do
      it (fnName <> " works") do
        liftEffect do
          let expected = "bar"
          ref <- Ref.new ""
          ee <- EventEmitter.new
          ee # fn fooH \val -> do
            Ref.write val ref
          void $ runEffectFn3 unsafeEmitFn2 ee "foo" expected
          val <- Ref.read ref
          val `shouldEqual` expected

  describe "subscribe functions" do
    let
      fns =
        [ "onSubscribe" /\ on
        , "onceSubscribe" /\ once
        , "prependListenerSubscribe" /\ prependListener
        , "prependOnceListenerSubscribe" /\ prependOnceListener
        ]
    for_ fns \(fnName /\ fn) -> do
      it (fnName <> " - normal call works") do
        liftEffect do
          let expected = "bar"
          ref <- Ref.new ""
          ee <- EventEmitter.new
          void $ ee # fn fooH \val -> do
            Ref.write val ref
          void $ runEffectFn3 unsafeEmitFn2 ee "foo" expected
          val <- Ref.read ref
          val `shouldEqual` expected
    for_ fns \(fnName /\ fn) -> do
      it (fnName <> " - unsubscribing before call works") do
        liftEffect do
          ref <- Ref.new ""
          ee <- EventEmitter.new
          remove <- ee # fn fooH \val -> do
            Ref.write val ref
          remove
          void $ runEffectFn3 unsafeEmitFn2 ee "foo" "bar"
          val <- Ref.read ref
          val `shouldEqual` ""

  describe "listener order" do
    it "runs listeners in registration order" do
      liftEffect do
        trace <- Ref.new ([] :: Array String)
        ee <- EventEmitter.new
        ee # on_ fooH \v -> Ref.modify_ (_ <> [ "first:" <> v ]) trace
        ee # on_ fooH \v -> Ref.modify_ (_ <> [ "second:" <> v ]) trace
        void $ runEffectFn3 unsafeEmitFn2 ee "foo" "x"
        Ref.read trace >>= shouldEqual [ "first:x", "second:x" ]

    it "prepends before the existing listeners of the same event" do
      liftEffect do
        trace <- Ref.new ([] :: Array String)
        ee <- EventEmitter.new
        ee # on_ fooH \v -> Ref.modify_ (_ <> [ "on-1:" <> v ]) trace
        ee # on_ fooH \v -> Ref.modify_ (_ <> [ "on-2:" <> v ]) trace
        ee # prependListener_ fooH \v -> Ref.modify_ (_ <> [ "prepend:" <> v ]) trace
        ee # prependOnceListener_ fooH \v -> Ref.modify_ (_ <> [ "prependOnce:" <> v ]) trace
        void $ runEffectFn3 unsafeEmitFn2 ee "foo" "x"
        void $ runEffectFn3 unsafeEmitFn2 ee "foo" "y"
        Ref.read trace >>= shouldEqual
          [ "prependOnce:x", "prepend:x", "on-1:x", "on-2:x"
          , "prepend:y", "on-1:y", "on-2:y"
          ]

    it "leaves listeners of other events alone" do
      liftEffect do
        fooSeen <- Ref.new 0
        barSeen <- Ref.new 0
        ee <- EventEmitter.new
        ee # on_ barH \_ -> Ref.modify_ (_ + 1) barSeen
        ee # on_ fooH \_ -> Ref.modify_ (_ + 1) fooSeen
        ee # prependListener_ fooH \_ -> Ref.modify_ (_ + 1) fooSeen
        void $ runEffectFn3 unsafeEmitFn2 ee "foo" "x"
        foo <- Ref.read fooSeen
        bar <- Ref.read barSeen
        foo `shouldEqual` 2
        bar `shouldEqual` 0
        let names = map nameOf (EventEmitter.eventNames ee)
        names `shouldEqual` [ "bar", "foo" ]

    it "runs once exactly once and in position" do
      liftEffect do
        trace <- Ref.new ([] :: Array String)
        ee <- EventEmitter.new
        ee # on_ fooH \_ -> Ref.modify_ (_ <> [ "on-1" ]) trace
        ee # once_ fooH \_ -> Ref.modify_ (_ <> [ "once" ]) trace
        ee # on_ fooH \_ -> Ref.modify_ (_ <> [ "on-2" ]) trace
        void $ runEffectFn3 unsafeEmitFn2 ee "foo" "x"
        void $ runEffectFn3 unsafeEmitFn2 ee "foo" "x"
        Ref.read trace >>= shouldEqual [ "on-1", "once", "on-2", "on-1", "on-2" ]

    it "handles nested emits" do
      liftEffect do
        trace <- Ref.new ([] :: Array String)
        ee <- EventEmitter.new
        ee # on_ fooH \v -> do
          Ref.modify_ (_ <> [ "outer:" <> v ]) trace
          void $ runEffectFn3 unsafeEmitFn2 ee "bar" "inner"
        ee # on_ barH \v -> Ref.modify_ (_ <> [ "inner:" <> v ]) trace
        void $ runEffectFn3 unsafeEmitFn2 ee "foo" "x"
        Ref.read trace >>= shouldEqual [ "outer:x", "inner:inner" ]

    it "tolerates a listener removing itself during emit" do
      liftEffect do
        count <- Ref.new 0
        remove <- Ref.new (pure unit)
        ee <- EventEmitter.new
        remover <- ee # on fooH \_ -> do
          Ref.modify_ (_ + 1) count
          join (Ref.read remove)
        Ref.write remover remove
        void $ runEffectFn3 unsafeEmitFn2 ee "foo" "x"
        void $ runEffectFn3 unsafeEmitFn2 ee "foo" "x"
        Ref.read count >>= shouldEqual 1

  describe "emit contract" do
    it "reports whether any listener exists for every arity" do
      liftEffect do
        ee <- EventEmitter.new
        none1 <- runEffectFn2 unsafeEmitFn1 ee "blank"
        none2 <- runEffectFn3 unsafeEmitFn2 ee "foo" "a"
        none3 <- runEffectFn4 unsafeEmitFn3 ee "pair" "a" "b"
        none4 <- runEffectFn5 unsafeEmitFn4 ee "triple" "a" "b" "c"
        for_ [ none1, none2, none3, none4 ] \found -> found `shouldEqual` false
        ee # on_ blankH (pure unit)
        ee # on_ fooH \_ -> pure unit
        ee # on_ pairH \_ _ -> pure unit
        ee # on_ tripleH \_ _ _ -> pure unit
        some1 <- runEffectFn2 unsafeEmitFn1 ee "blank"
        some2 <- runEffectFn3 unsafeEmitFn2 ee "foo" "a"
        some3 <- runEffectFn4 unsafeEmitFn3 ee "pair" "a" "b"
        some4 <- runEffectFn5 unsafeEmitFn4 ee "triple" "a" "b" "c"
        for_ [ some1, some2, some3, some4 ] \found -> found `shouldEqual` true

    it "forwards the emitted arguments for each arity" do
      liftEffect do
        one <- Ref.new ""
        two <- Ref.new ""
        three <- Ref.new ""
        ee <- EventEmitter.new
        ee # on_ fooH \v -> Ref.write v one
        ee # on_ pairH \a b -> Ref.write (a <> "/" <> b) two
        ee # on_ tripleH \a b c -> Ref.write (a <> "/" <> b <> "/" <> c) three
        void $ runEffectFn3 unsafeEmitFn2 ee "foo" "one"
        void $ runEffectFn4 unsafeEmitFn3 ee "pair" "a" "b"
        void $ runEffectFn5 unsafeEmitFn4 ee "triple" "a" "b" "c"
        Ref.read one >>= shouldEqual "one"
        Ref.read two >>= shouldEqual "a/b"
        Ref.read three >>= shouldEqual "a/b/c"

  describe "bookkeeping" do
    it "listenerCount reflects subscriptions and removal" do
      liftEffect do
        ee <- EventEmitter.new
        count0 <- EventEmitter.listenerCount ee "foo"
        count0 `shouldEqual` 0
        remove1 <- ee # on fooH \_ -> pure unit
        _ <- ee # on fooH \_ -> pure unit
        count2 <- EventEmitter.listenerCount ee "foo"
        count2 `shouldEqual` 2
        remove1
        count1 <- EventEmitter.listenerCount ee "foo"
        count1 `shouldEqual` 1

    it "reads and writes the max listeners, including unlimited" do
      liftEffect do
        ee <- EventEmitter.new
        initial <- EventEmitter.getMaxListeners ee
        initial `shouldEqual` 10
        EventEmitter.setMaxListeners 3 ee
        updated <- EventEmitter.getMaxListeners ee
        updated `shouldEqual` 3
        EventEmitter.setUnlimitedListeners ee
        unlimited <- EventEmitter.getMaxListeners ee
        unlimited `shouldEqual` 0

    it "notifies newListener and removeListener" do
      liftEffect do
        trace <- Ref.new ([] :: Array String)
        ee <- EventEmitter.new
        ee # on_ EventEmitter.newListenerH \name -> Ref.modify_ (_ <> [ "new:" <> nameOf name ]) trace
        ee # on_ EventEmitter.removeListenerH \name -> Ref.modify_ (_ <> [ "remove:" <> nameOf name ]) trace
        remove <- ee # on fooH \_ -> pure unit
        remove
        -- Registering the `removeListener` listener itself also fires
        -- `newListener`; keep only the `foo` notifications.
        events <- Ref.read trace
        let fooEvents = Array.filter (\name -> name == "new:foo" || name == "remove:foo") events
        fooEvents `shouldEqual` [ "new:foo", "remove:foo" ]
