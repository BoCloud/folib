const webpack = require('webpack')
const CompressionPlugin = require('compression-webpack-plugin')
const zlib = require('zlib')
const Timestamp = new Date().getTime()

module.exports = {
	publicPath: './',
	outputDir: '../folib-web-core/src/main/resources/ui',
	assetsDir: 'static',
	runtimeCompiler: true,
	productionSourceMap: false, // 不生成 source map
	chainWebpack: config => {
		config
			.plugin('html')
			.tap(args => {
				args[0].title = '制品库'
				return args
			})
	},
	configureWebpack: {
		plugins: [
			new webpack.optimize.LimitChunkCountPlugin({
				maxChunks: 5, // 限制最多生成 5 个 chunk 文件
			}),
			// 压缩成 .gz 文件
			new CompressionPlugin({
				filename: '[path][base].gz',
				algorithm: 'gzip',
				test: /\.js$|\.css$|\.html$/,
				threshold: 10240,
				minRatio: 0.8
			}),
			// 压缩成 .br 文件，如果 zlib 报错无法解决，可以注释这段使用代码，一般本地没问题，需要注意线上服务器会可能发生找不到 zlib 的情况。
			new CompressionPlugin({
				filename: '[path][base].br',
				algorithm: 'brotliCompress',
				test: /\.(js|css|html|svg)$/,
				compressionOptions: {
					params: {
						[zlib.constants.BROTLI_PARAM_QUALITY]: 11
					}
				},
				threshold: 10240,
				minRatio: 0.8
			})
		]
	},
	devServer: {
		// development server port 8000
		port: 9528,
		proxy: {
			'/api': {
				// target: 'http://f18811435520.e2.luyouxia.net:20626',
				// target: 'http://10.50.8.55:38080',
				// target: 'http://10.50.8.82:38080',
				// target: 'https://demo2.folib.com', 
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
