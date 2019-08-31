// NOTE: css classes within single quotes are not picked up this purgecss
// filter and will be removed

module.exports = {
  plugins: [
    // tailwind plugin with config file
    require('tailwindcss')('./tailwind.js'),
    // for adding vendor prefixes
    require('autoprefixer'),
    // only apply purgecss if in production
    ...(process.env.NODE_ENV === 'production'
      ? [
          // removes all unused css classes
          require('@fullhuman/postcss-purgecss')({
            // files to scan for classes
            content: ['./src/**/*.html', './src/**/*.elm'],
            // regex to look for classes
            defaultExtractor: content =>
              // match in double quotes
              (
                content.match(
                  new RegExp(`(?<!")"[A-Za-z0-9-_: ]+"(?!")`, 'g')
                ) || []
              )
                // remove quotes and extraneous spaces from matches
                .map(a => a.slice(1, a.length - 1).trim())
                // get each word
                .map(a => a.split(' '))
                // join arrays together
                .reduce((a, b) => a.concat(b), []),
            // whitelist certain elements/classes
            whitelist: [''],
          }),
        ]
      : // otherwise dont run purgecss
        []),
  ],
};
