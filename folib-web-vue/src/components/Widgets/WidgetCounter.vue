<template>

	<!-- Counter widget -->
	<a-card :bordered="false" class="widget-1 counter-widget">
		<a-statistic
			:title="title"
			:value="value"
			:prefix="prefix"
			:suffix="suffix"
			:precision="precision"
			class="text-success"
			:class="'text-' + status"
		>
		<span v-if="tag" slot="formatter">
			<span v-if="screenWidth <= 1366">
				<a-tooltip>
					<template slot="title">
						{{ value }}
					</template>
					{{ value.substr(0, 10)  + '...'}}
					</a-tooltip>
			</span>
			<span v-else-if="value.length > 20">
				<a-tooltip>
					<template slot="title">
						{{ value }}
					</template>
					{{ value.substr(0, 20) + '...'}}
					</a-tooltip>
			</span>
			<span v-else>{{ value }}</span>
		</span>
		</a-statistic>
		
		<div class="icon" v-html="icon" v-if="icon">
		</div>
		<div class="icon" v-if="aIcon">
			<a-icon :type="aIcon" :theme="theme?theme:'outlined'" style="font-size: 24px;"/>
		</div>
		<div class="icon icon-src" v-if="src">
			<img :src="src" width="48" alt="icon"/>
		</div>
	</a-card>
	<!-- / Counter widget -->

</template>

<script>

	export default ({
		props: {
			title: {
				type: String,
				default: "",
			},
			value: {
				type: [String, Number],
				default: "",
			},
			prefix: {
				type: String,
				default: "",
			},
			suffix: {
				type: String,
				default: "",
			},
			icon: {
				type: String,
				default: "",
			},
			status: {
				type: String,
				default: "success",
			},
			precision: {
				type: Number,
				default: 0,
			},
			aIcon: {
				type: String,
				default: "",
			},
			theme: {
				type: [String],
				default: "",
			},
			src: {
				type: String,
				default: "",
			},
			tag: {
				type: Boolean,
				default: false
			},
		},
		data() {
			return {
				screenWidth:0,
			}
		},
		methods: {
			updateScreenWidth() {
				this.screenWidth = window.innerWidth;
			}
		},
		mounted() {
			// 初始化宽度
			this.updateScreenWidth();
			// 监听窗口大小变化
			window.addEventListener('resize', this.updateScreenWidth);
		},
		beforeDestroy() {
			// 移除监听器
			window.removeEventListener('resize', this.updateScreenWidth);
		}
	})

</script>
<style lang="scss" scoped>
	.counter-widget{
		.icon-src {
			background-color: unset;
		}
	}
</style>