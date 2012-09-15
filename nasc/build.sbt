seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

name := "scarla"

version := "1.0"

scalaSource in Compile <<= baseDirectory(_ / "src" / "nasc")