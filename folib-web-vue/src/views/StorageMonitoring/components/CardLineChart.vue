<template>

	<a-card :bordered="false" class="dashboard-bar-line header-solid">
		<template #title>
			<h6>{{$t('StorageMonitoring.ArtifactsCount')}}</h6>
			<p>
				{{$t('StorageMonitoring.ThanLastUpdate')}}
				<span v-if="storageMonitoringData.artifactsCountDiff !== 0 && ! storageMonitoringData.artifactsCountDiff">-</span>
				<span v-else-if="storageMonitoringData.artifactsCountDiff >= 0">+ {{ storageMonitoringData.artifactsCountDiff }}</span>
				<span v-else> {{ storageMonitoringData.artifactsCountDiff }}</span>
			</p>
		</template>
		<template #extra>
			<a-badge color="primary" class="badge-dot-primary" :text="$t('StorageMonitoring.FilesCount')" />
			<a-badge color="primary" class="badge-dot-secondary" :text="$t('StorageMonitoring.ArtifactsCount')" />
		</template>
		<chart-line :height="310" :data="lineChartData"></chart-line>
		<div class="chart-tips">{{ $t('StorageMonitoring.LastTenArtifactUpdates') }}</div>
	</a-card>

</template>

<script>

	import ChartLine from '@/components/Charts/ChartLine' ;
	import {
			getLayoutType,
			getFileType,
			fileSizeConver,
			fileSizeConverUnit,
			formateDate,
	} from "@/utils/layoutUtil"

	export default ({
		props: {
			data: {
				type: Object,
				default: function () {
					return {
					} 
				}
			},
			storageMonitoringData: {
				type: Object,
				default: {}
			},
		},
		components: {
			ChartLine,
		},
		data() {
			return {

				// Data for line chart.
				lineChartData: this.data
			}
		},
		methods: {
			fileSizeConver(size) {
				if (size >= 0) {
					return fileSizeConver(size)
				}
			},
		}
	})

</script>

<style lang="scss" scoped>
.dashboard-bar-line {
	height: 100%;
	.chart-tips {
		width: 100%;
		text-align: center;
	}
}
</style>