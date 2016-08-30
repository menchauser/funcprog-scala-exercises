import stream.Empty

val s = stream.Stream.cons({println("Hi"); 5}, Empty)

s.headOption
s.headOption

val ss = stream.Cons(() => {println("Hi"); 5}, () => Empty)

ss.headOption
ss.headOption
