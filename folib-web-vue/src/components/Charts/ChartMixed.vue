<template>
	<div>
		<!-- Mixed chart -->
		<canvas ref="chart" class="chart-mixed" :style="{'height': height + 'px'}"></canvas>
		<!-- / Mixed chart -->
	</div>
</template>

<script>
	import { Chart, registerables } from 'chart.js';
	Chart.register(...registerables);

	export default ({
		props: {
			data: {
				type: Object,
				default: function () {
					return {
						labels: ["A", "B", "C"],
						datasets: [
							{
								type: "bar",
								label: "",
								weight: 5,
								tension: 0.4,
								borderWidth: 0,
								pointBackgroundColor: "#141414",
								borderColor: "#141414",
								backgroundColor: '#141414',
								borderRadius: 4,
								borderSkipped: false,
								data: [10, 20, 30],
								maxBarThickness: 10,
							},
						],
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
		watch: {
			data: {
				handler(newVal) {
					if (this.chart) {
						this.updateChart(newVal);
					}
				},
				deep: true,
			},
		},
		mounted () { 
    	let ctx = this.$refs.chart.getContext("2d");

			var gradientStroke1 = ctx.createLinearGradient(0, 230, 0, 50);

			gradientStroke1.addColorStop(1, 'rgba(24, 144, 255, .3)');
			gradientStroke1.addColorStop(0.2, 'rgba(24, 144, 255, 0)');
			gradientStroke1.addColorStop(0, 'rgba(24, 144, 255, 0)'); // Primary color

			this.chart = new Chart(ctx, {
					data: this.data,
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