<template>
	<div>
		<!-- Line chart full -->
		<canvas ref="chart" class="chart-line-full" :style="{'height': height + 'px'}"></canvas>
		<!-- / Line chart full -->
	</div>
</template>

<script>
	import { Chart, registerables } from 'chart.js';
	Chart.register(...registerables);

	export default ({
		props: [
			'data',
			'height',
		],
		data(){
			return {
				chart: null,
			} ;
		},
		mounted () { 
    		let ctx = this.$refs.chart.getContext("2d");

			var gradientStroke = ctx.createLinearGradient(0, 230, 0, 50);

			gradientStroke.addColorStop(1, 'rgba(255,255,255,0.3)');
			gradientStroke.addColorStop(0.2, 'rgba(72,72,176,0.0)');
			gradientStroke.addColorStop(0, 'rgba(203,12,159,0)'); //purple colors

			this.data.datasets = this.data.datasets.map( ( item ) => {
				item.backgroundColor = gradientStroke ;

				return item ;
			} )

			this.chart = new Chart(ctx, {
				type: "line",
				data: this.data,
				options: {
					responsive: true,
					maintainAspectRatio: false,
					plugins: {
						legend: {
							display: false,
						}
					},
					interaction: {
						intersect: false,
						mode: 'index',
					},
					scales: {
						y: {
							grid: {
							drawBorder: false,
							display: false,
							drawOnChartArea: false,
							drawTicks: false,
							},
							ticks: {
							display: false
							}
						},
						x: {
							grid: {
							drawBorder: false,
							display: false,
							drawOnChartArea: false,
							drawTicks: false,
							},
							ticks: {
							display: false
							}
						},
					},
				},
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