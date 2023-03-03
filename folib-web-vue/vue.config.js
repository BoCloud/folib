module.exports = {
	publicPath: './',
	outputDir: '../folib-web-core/src/main/resources/ui',
	assetsDir: 'static',
	runtimeCompiler: true,
	chainWebpack: config => {
		config
			.plugin('html')
			.tap(args => {
				args[0].title = 'FoLibrary'
				return args
			})
	},
	devServer: {
		// development server port 8000
		port: 9527,
		proxy: {
			'/api': {
				target: 'http://localhost:38080',
				// target: 'http://xpboot.cn:38080',
				// target: 'http://10.10.33.145:38080',
				pathRewrite: {
					'^/': '/'
				},
				timeout: 15 * 60 * 1000,
				proxyTimeout: 15 * 60 * 1000,
			}
		}
	},
}