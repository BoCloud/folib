<template>
	<div>
		<!-- Bubble chart -->
		<canvas ref="chart" class="chart-bubble" :style="{'height': height + 'px'}"></canvas>
		<!-- / Bubble chart -->
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
				type: "bubble",
				data: {
					labels: ['0', '10', '20', '30', '40', '50', '60', '70', '80', '90'],
					datasets: [{
						label: 'Dataset 1',
						data: [{
							x: 100,
							y: 0,
							r: 10
							}, {
							x: 60,
							y: 30,
							r: 20
							}, {
							x: 40,
							y: 350,
							r: 10
							}, {
							x: 80,
							y: 80,
							r: 10
							}, {
							x: 20,
							y: 30,
							r: 15
							}, {
							x: 0,
							y: 100,
							r: 5
						}],
						backgroundColor: '#1890FF',
					},{
						label: 'Dataset 2',
						data: [{
							x: 70,
							y: 40,
							r: 10
							}, {
							x: 30,
							y: 60,
							r: 20
							}, {
							x: 10,
							y: 300,
							r: 25
							}, {
							x: 60,
							y: 200,
							r: 10
							}, {
							x: 50,
							y: 300,
							r: 15
							}, {
							x: 20,
							y: 350,
							r: 5
						}],
						backgroundColor: '#141414',
					}]
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