module.exports = {
  "entry": {
    "chescala-react-fastopt": ["/Users/hansblackcat/Documents/Git/chescala-react/target/scala-2.13/scalajs-bundler/main/chescala-react-fastopt-entrypoint.js"]
  },
  "output": {
    "path": "/Users/hansblackcat/Documents/Git/chescala-react/target/scala-2.13/scalajs-bundler/main",
    "filename": "[name]-library.js",
    "library": "ScalaJSBundlerLibrary",
    "libraryTarget": "var"
  },
  "mode": "development",
  "devtool": "source-map",
  "module": {
    "rules": [{
      "test": new RegExp("\\.js$"),
      "enforce": "pre",
      "use": ["source-map-loader"]
    }]
  }
}