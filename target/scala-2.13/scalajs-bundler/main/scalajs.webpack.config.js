module.exports = {
  "entry": {
    "chescala-react-opt": ["/Users/hansblackcat/Documents/Git/chescala-react/target/scala-2.13/scalajs-bundler/main/chescala-react-opt.js"]
  },
  "output": {
    "path": "/Users/hansblackcat/Documents/Git/chescala-react/target/scala-2.13/scalajs-bundler/main",
    "filename": "[name]-bundle.js"
  },
  "mode": "production",
  "devtool": "source-map",
  "module": {
    "rules": [{
      "test": new RegExp("\\.js$"),
      "enforce": "pre",
      "use": ["source-map-loader"]
    }]
  }
}