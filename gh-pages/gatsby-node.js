var rucksack = require('rucksack-css')
var lost = require("lost")
var cssnext = require("postcss-cssnext")
var fs = require('fs-extra-promise') //install this package
var sm = require('sitemap') // install this package

function pagesToSitemap(pages) {
  var urls = pages.map(function(p) {
    if (p.path !== undefined) {
      return {
        url: p.path,
        changefreq: 'daily',
        priority: 0.7
      }
    }
  })
  // remove undefined (template pages)
  return urls.filter(function(u) { return u !== undefined})
}

function generateSiteMap(pages) {
  var sitemap = sm.createSitemap({
    hostname: 'http://lab.ar90n.net',
    cacheTime: '60000',
    urls: pagesToSitemap(pages),
  })
  console.log('Generating sitemap.xml')
  fs.writeFileSync(
    `${__dirname}/public/sitemap.xml`,
    sitemap.toString()
  )
}

module.exports.modifyWebpackConfig = function(config, env) {
    config.merge({
        postcss: [
            lost(),
            rucksack(),
            cssnext({
                browsers: ['>1%', 'last 2 versions']
            })
        ]
    })

    config.loader('svg', {
       test: /\.(svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
       loader: 'file-loader',
    })

    return config
};

module.exports.postBuild = function(pages, callback) {
  generateSiteMap(pages)
  callback()
}
