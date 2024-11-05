module.exports = {
	publicPath: './',
	outputDir: '../folib-web-core/src/main/resources/ui',
	assetsDir: 'static',
	runtimeCompiler: true,
	chainWebpack: config => {
		config
			.plugin('html')
			.tap(args => {
				args[0].title = '制品库'
				return args
			})
	},
	devServer: {
		// development server port 8000
		port: 9528,
		proxy: {
			'/api': {
				// target: 'http://f18811435520.e2.luyouxia.net:20626',
				// target: 'http://10.50.8.55:38080',
				target: 'http://127.0.0.1:38080',
				//target: 'http://192.168.42.128:38080',
				// target: 'http://xpboot.cn:38080',
				// target: 'http://10.10.33.145:38080',
				pathRewrite: {
					'^/': '/'
				},
				timeout: 15 * 60 * 1000,
				proxyTimeout: 15 * 60 * 1000,
			},
			'/dependency': {
				target: 'http://10.10.28.61:9527',
				// target: 'http://192.168.5.101:8081',
				// target: 'http://xpboot.cn:38080',
				// target: 'http://10.10.33.145:38080',
				pathRewrite: {
					'^/dependency': '/api'
				},
			}
		}
	},
}
