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
	configureWebpack: config => {
		const plugins = [];
		// 始终启用 chunk 限制
		plugins.push(
			new webpack.optimize.LimitChunkCountPlugin({
				maxChunks: 5 // 限制最多生成 5 个 chunk 文件
			})
		);
		// 仅在生产环境启用压缩
		if (process.env.NODE_ENV === 'production') {
			// Gzip 压缩
			plugins.push(
				new CompressionPlugin({
					filename: '[path][base].gz',
					algorithm: 'gzip',
					test: /\.js$|\.css$|\.html$/,
					threshold: 10240,          // 只压缩大于10KB的文件
					minRatio: 0.8,
					deleteOriginalAssets: false // 保留原始文件
				})
			);
			// Brotli 压缩（仅在支持的环境启用）
			try {
				if (zlib.brotliCompress) {
					plugins.push(
						new CompressionPlugin({
							filename: '[path][base].br',
							algorithm: 'brotliCompress',
							test: /\.(js|css|html|svg)$/,
							compressionOptions: {
								params: {
									// 降低压缩级别以提升速度（1-11，11最高）
									[zlib.constants.BROTLI_PARAM_QUALITY]: 9
								}
							},
							threshold: 10240,
							minRatio: 0.8,
							deleteOriginalAssets: false
						})
					);
				}
			} catch (error) {
				console.warn('Brotli 压缩不可用，已跳过:', error.message);
			}
		}

		return { plugins };
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
