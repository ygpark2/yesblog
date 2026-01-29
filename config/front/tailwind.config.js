const path = require("path");
const root = path.resolve(__dirname, "..", "..");

module.exports = {
  content: [
    path.join(root, "templates/**/*.hamlet"),
    path.join(root, "templates/**/*.lucius"),
    path.join(root, "templates/**/*.julius"),
    path.join(root, "static/**/*.{js,css}"),
    path.join(root, "src/**/*.hs"),
  ],
  theme: {
    extend: {
      colors: {
        brand: "#0f172a",
      },
    },
  },
};
