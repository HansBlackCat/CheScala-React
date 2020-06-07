if (process.env.NODE_ENV === "production") {
    const opt = require("./chescala-react-opt.js");
    opt.main();
    module.exports = opt;
} else {
    var exports = window;
    exports.require = require("./chescala-react-fastopt-entrypoint.js").require;
    window.global = window;

    const fastOpt = require("./chescala-react-fastopt.js");
    fastOpt.main()
    module.exports = fastOpt;

    if (module.hot) {
        module.hot.accept();
    }
}
