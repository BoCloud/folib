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
		</a-statistic>
		
		<div class="icon" v-html="icon" v-if="icon">
		</div>
		<div class="icon" v-if="aIcon">
			<a-icon :type="aIcon" :theme="theme?theme:'outlined'" style="font-size: 24px;"/>
		</div>
		<div class="icon icon-src" v-if="src">
			<img :src="src" width="48"/>
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
			}
		},
		data() {
			return {
			}
		},
	})

</script>
<style lang="scss" scoped>
	.counter-widget{
		.icon-src {
			background-color: unset;
		}
	}
</style>