<template>
	<div>
		<!-- Polar chart -->
		<canvas ref="chart" class="chart-polar" :style="{'height': height + 'px'}"></canvas>
		<!-- / Polar chart -->
	</div>
</template>

<script>
	import { Chart, registerables } from 'chart.js';
	Chart.register(...registerables);

	export default ({
		props: [
		],
		data(){
			return {
				height: 500,
			} ;
		},
		mounted () { 
    		let ctx = this.$refs.chart.getContext("2d");

			this.chart = new Chart(ctx, {
				type: "polarArea",
				data: {
					labels: [
					'Red',
					'Green',
					'Yellow',
					'Grey',
					'Blue'
					],
					datasets: [{
					label: 'My First Dataset',
					data: [11, 16, 7, 3, 14],
					backgroundColor: ['#FADB14', '#1890FF', '#52C41A', '#B37FEB', '#F5222D'],
					}]
				},
				options: {
					plugins: {
					legend: {
						display: false,
					}
					}
				}
			});
		},
		// Right before the component is destroyed,
		// also destroy the chart.
		beforeDestroy: function () {
			this.chart.destroy() ;
		},
	})

</script>

<style lang="scss" scoped>
</style>