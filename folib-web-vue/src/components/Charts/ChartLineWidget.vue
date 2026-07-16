<template>
	<div>
		<!-- Line chart widget -->
		<canvas ref="chart" class="chart-line-widget" :style="{'height': height + 'px'}"></canvas>
		<!-- Line chart widget -->
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

			gradientStroke.addColorStop(1, 'rgba(24, 144, 255, .1)');
			gradientStroke.addColorStop(0.2, 'rgba(24, 144, 255, 0)');
			gradientStroke.addColorStop(0, 'rgba(24, 144, 255, 0)'); // Primary color

			this.data.backgroundColor = gradientStroke ;

			this.chart = new Chart(ctx, {
				type: "line",
				data: {
					labels: ["Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
					datasets: [{
						label: "Visitors",
						tension: 0.5,
						borderWidth: 0,
						pointRadius: 0,
						borderColor: "#1890FF",
						borderWidth: 2,
						backgroundColor: gradientStroke,
						data: [50, 45, 60, 60, 80, 65, 90, 80, 100],
						maxBarThickness: 6,
						fill: true
					}],
				},
     			options: {
					layout: {
						padding: {
							top: 10,
							right: 10,
							left: 10,
							bottom: 10,
						},
					},
					responsive: true,
					maintainAspectRatio: false,
					plugins: {
						legend: {
							display: false,
						},
					},
					tooltips: {
						enabled: true,
						mode: "index",
						intersect: false,
					},
					scales: {
						y: {
							grid: {
								drawBorder: false,
								display: false,
								drawOnChartArea: true,
								drawTicks: false,
								borderDash: [5, 5]
							},
							ticks: {
								suggestedMin: 0,
								suggestedMax: 1000,
								display: true,
								padding: 5,
              					color: '#9ca2b7',
								font: {
									size: 13,
									lineHeight: 1.2,
									weight: '400',
									family: "Open Sans",
								},
							},
						},
						x: {
							grid: {
								drawBorder: false,
								display: true,
								drawOnChartArea: true,
								drawTicks: false,
								borderDash: [5, 5]
							},
							ticks: {
								display: true,
								padding: 10,
								color: '#9ca2b7',
								font: {
									size: 13,
									lineHeight: 1.2,
									weight: '400',
									family: "Open Sans",
								},
							},
						},
					},
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