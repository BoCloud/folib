<template>
	<div>
		<!-- Doughnut chart -->
		<canvas ref="chart" class="chart-doughnut" :style="{'height': height + 'px'}"></canvas>
		<!-- / Doughnut chart -->
	</div>
</template>

<script>
	import { Chart, registerables } from 'chart.js';
	Chart.register(...registerables);

	export default ({
		props: {
			chartData: {
				type: Object,
				default: function () {
					return {
						labels: ['A', 'B', 'C'],
						datasets: [
							{
								label: "Projects",
								weight: 9,
								cutout: 60,
								tension: 0.9,
								pointRadius: 2,
								borderWidth: 2,
								backgroundColor: ['#FADB14', '#B37FEB', '#52C41A'],
								data: [15, 20, 12],
								fill: false
							}
						]
					}
				}
			},
			height: {
				type: Number,
				default: 300,
			},
		},
		data(){
			return {
				
			} ;
		},
		mounted () { 
    	let ctx = this.$refs.chart.getContext("2d");
			this.chart = new Chart(ctx, {
				type: "doughnut",
				data: this.chartData,
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
						display: false,
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