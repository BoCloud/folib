<template>
	<div>
		<!-- Line chart 2 -->
		<canvas ref="chart" class="chart-line-2" :style="{'height': height + 'px'}"></canvas>
		<!-- / Line chart 2 -->
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
				type: "line",
				data: {
					labels: ["Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
					datasets: [{
						label: "Organic Search",
						tension: 0.4,
						borderWidth: 0,
						pointRadius: 2,
						pointBackgroundColor: "#1890FF",
						borderColor: "#1890FF",
						borderWidth: 3,
						data: [50, 40, 300, 220, 500, 250, 400, 230, 500],
						maxBarThickness: 6
					},
					{
						label: "Referral",
						tension: 0.4,
						borderWidth: 0,
						pointRadius: 2,
						pointBackgroundColor: "#141414",
						borderColor: "#141414",
						borderWidth: 3,
						data: [30, 90, 40, 140, 290, 290, 340, 230, 400],
						maxBarThickness: 6
					},
					{
						label: "Direct",
						tension: 0.4,
						borderWidth: 0,
						pointRadius: 2,
						pointBackgroundColor: "#B37FEB",
						borderColor: "#B37FEB",
						borderWidth: 3,
						data: [40, 80, 70, 90, 30, 90, 140, 130, 200],
						maxBarThickness: 6
					},
					],
				},
     			options: {
					layout: {
						padding: {
							top: 30,
							right: 15,
							left: 10,
							bottom: 5,
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
								display: true,
								drawOnChartArea: true,
								drawTicks: false,
								borderDash: [5, 5]
							},
							ticks: {
								display: true,
								padding: 10,
								color: '#b2b9bf',
								font: {
									size: 11,
									family: "Open Sans",
									style: 'normal',
									lineHeight: 2
								},
							},
						},
						x: {
							grid: {
								drawBorder: false,
								display: true,
								drawOnChartArea: true,
								drawTicks: true,
								borderDash: [5, 5]
							},
							ticks: {
								display: true,
								color: '#b2b9bf',
								padding: 10,
								font: {
									size: 11,
									family: "Open Sans",
									style: 'normal',
									lineHeight: 2
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