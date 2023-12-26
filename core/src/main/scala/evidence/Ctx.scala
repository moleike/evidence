package evidence

import scala.util.NotGiven

type :*[+H[_, _], E]

type :?[H[_, _], E] = Ctx.In[H, E]

// a heterogeneous list of marker and handler pairs (so-called evidence)
enum Ctx[+E]:
  case CNil extends Ctx[Nothing]

  case CCons[+H[_, _], E, EE, Ans](
      marker: Marker[Ans],
      handler: H[EE, Ans],
      trans: Ctx[E] => Ctx[EE],
      tail: Ctx[E]
  ) extends Ctx[H :* E]

object Ctx:

    trait SubCtx[+H[_, _]]:
      type T
      def value: Ctx[H :* T]

    trait In[+H[_, _], E]:
      def subCtx(ctx: Ctx[E]): SubCtx[H]

    object In:
      given [H[_, _], E]: In[H, H :* E] with
          def subCtx(ctx: Ctx[H :* E]): SubCtx[H] = new SubCtx[H]:
            type T = E
            val value = ctx

      given [H[_, _], G[_, _], E, Ans](using
          NotGiven[H[E, Ans] =:= G[E, Ans]],
          In[G, E]
      ): In[G, H :* E] with
          def subCtx(ctx: Ctx[H :* E]): SubCtx[G] = ctx match
            case Ctx.CCons(_, _, _, xs) => summon[In[G, E]].subCtx(xs)
