package evidence
package effect

type Reader[+A]    = [E, Ans] =>> internal.Reader[A, E, Ans]
type State[A]      = [E, Ans] =>> internal.State[A, E, Ans]
type Except[-A]    = [E, Ans] =>> internal.Except[A, E, Ans]
type Writer[-A]    = [E, Ans] =>> internal.Writer[A, E, Ans]
type Chronicle[-A] = [E, Ans] =>> internal.Chronicle[A, E, Ans]
type NonDet        = [E, Ans] =>> internal.NonDet[E, Ans]
type Console       = [E, Ans] =>> internal.Console[E, Ans]

val Reader    = internal.Reader
val State     = internal.State
val Except    = internal.Except
val Writer    = internal.Writer
val Chronicle = internal.Chronicle
val NonDet    = internal.NonDet
val Console   = internal.Console
