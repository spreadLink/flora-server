const defaultTheme = require('tailwindcss/defaultTheme')

module.exports = {
  mode: "jit",
  purge: ["./js/**/*.js", "../src/FloraWeb/**/*.*hs"],
  darkMode: 'media',
  theme: {
    extend: {
      fontFamily: {
        sans: ['Inter var', ...defaultTheme.fontFamily.sans],
      },
    },
  },
  variants: {
    extend: {},
  },
  plugins: [],
  colors: {
    background: {
      primary: 'var(--bg-background-primary)',
      secondary: 'var(--bg-background-secondary)',
      tertiary: 'var(--bg-background-tertiary)',
    }
  }
};
