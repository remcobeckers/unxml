resolvers += Resolver.url("bintray-sbt-plugin-releases", url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")
addSbtPlugin("com.scalapenos" % "sbt-prompt" % "0.1")
addSbtPlugin("io.spray" % "sbt-boilerplate" % "0.5.9")
addSbtPlugin("me.lessis" % "bintray-sbt" % "0.1.2")
