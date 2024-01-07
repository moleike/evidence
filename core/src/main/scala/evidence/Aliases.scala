package evidence

type Reader[+A] = [E, Ans] =>> effect.Reader[A, E, Ans]
type State[A] = [E, Ans] =>> effect.State[A, E, Ans]
type Except[-A] = [E, Ans] =>> effect.Except[A, E, Ans]
type Writer[-A] = [E, Ans] =>> effect.Writer[A, E, Ans]
type Chronicle[-A] = [E, Ans] =>> effect.Chronicle[A, E, Ans]
type NonDet = [E, Ans] =>> effect.NonDet[E, Ans]
type Console = [E, Ans] =>> effect.Console[E, Ans]

val Reader = effect.Reader
val State = effect.State
val Except = effect.Except
val Writer = effect.Writer
val Chronicle = effect.Chronicle
val NonDet = effect.NonDet
val Console = effect.Console
