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
		props: [
		],
		data(){
			return {
				height: 300,
			} ;
		},
		mounted () { 
    		let ctx = this.$refs.chart.getContext("2d");

			this.chart = new Chart(ctx, {
				type: "doughnut",
				data: {
					labels: ['Creative Tim', 'Github', 'Bootsnipp', 'Dev.to', 'Codeinwp'],
					datasets: [{
						label: "Projects",
						weight: 9,
						cutout: 60,
						tension: 0.9,
						pointRadius: 2,
						borderWidth: 2,
						backgroundColor: ['#FADB14', '#B37FEB', '#52C41A', '#1890FF', '#F5222D'],
						data: [15, 20, 12, 60, 20],
						fill: false
					}],
				},
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