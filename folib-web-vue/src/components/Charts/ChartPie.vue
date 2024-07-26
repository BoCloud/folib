<template>
	<div>
		<!-- Pie chart -->
		<canvas ref="chart" class="chart-pie" :style="{'height': height + 'px'}"></canvas>
		<!-- / Pie chart -->
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
							cutout: 0,
							tension: 0.9,
							pointRadius: 2,
							borderWidth: 2,
							backgroundColor: ['#52C41A', '#1890FF'],
							data: [15, 20],
							fill: false
							}
						],
					}
				}
			},
			height: {
				type: Number,
				default: 300,
			},
		},
		watch: {
			chartData: {
				handler(newVal) {
					if (this.chart) {
						this.updateChart(newVal);
					}
				},
				deep: true,
			},
		},
		data(){
			return {
			} ;
		},
		mounted () { 
    		let ctx = this.$refs.chart.getContext("2d");

			this.chart = new Chart(ctx, {
				type: "pie",
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
		methods: {
			updateChart(newData) {
				this.chart.data = newData;
				this.chart.update();
			}
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