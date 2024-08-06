<template>

	<a-card :bordered="false" class="dashboard-mixed-chart">
		<ChartMixed :height="220" :data="data"/>
		<div class="chart-tips">{{ $t('StorageMonitoring.LastTenUpdates') }}</div>
		<div class="card-title">
			<h6>{{$t('StorageMonitoring.FilesSize')}}</h6>
			<p>
				{{$t('StorageMonitoring.ThanLastUpdate')}}
				<span v-if="storageMonitoringData.filesSizeDiff !== 0 && ! storageMonitoringData.filesSizeDiff">-</span>
				<span v-else-if="storageMonitoringData.filesSizeDiff >= 0">+ {{ storageMonitoringData.filesSizeDiff }}</span>
				<span v-else>- {{ storageMonitoringData.filesSizeDiff }}</span>
			</p>
		</div>
		<a-row class="card-footer" type="flex" justify="center" align="top">
			<a-col :span="6" v-if="!storageMonitoringData.storageId">
				<h4>{{ storageDeviceCount }}</h4>
				<span>{{$t('StorageMonitoring.StorageDeviceCount')}}</span>
			</a-col>
			<a-col :span="6" v-if="!storageMonitoringData.storageId">
				<h4>{{storageMonitoringData.storageCount ? storageMonitoringData.storageCount : '-'}}</h4>
				<span>{{$t('StorageMonitoring.StorageCount')}}</span>
			</a-col>
			<a-col :span="storageMonitoringData.storageId?12:6">
				<h4>{{storageMonitoringData.repositoryCount ? storageMonitoringData.repositoryCount : '-'}}</h4>
				<span>{{$t('StorageMonitoring.RepositoryCount')}}</span>
			</a-col>
			<a-col :span="storageMonitoringData.storageId?12:6">
				<h4>{{storageMonitoringData.artifactsDownloadedCount ? storageMonitoringData.artifactsDownloadedCount : '-'}}</h4>
				<span>{{$t('StorageMonitoring.DownloadedCount')}}</span>
			</a-col>
		</a-row>
	</a-card>

</template>

<script>

  import ChartMixed from '@/components/Charts/ChartMixed' ;
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
							{
								type: "line",
								label: "Referral",
								tension: 0.4,
								borderWidth: 0,
								pointRadius: 0,
								pointBackgroundColor: "#1890FF",
								borderColor: "#1890FF",
								borderWidth: 3,
								data: [10, 20, 30],
								fill: true,
							}
						],
					}
				}
			},
			storageMonitoringData: {
				type: Object,
				default: {}
			},
			storageDeviceCount: {
				type: [ Number, String],
				default: '-'
			},
		},
		components: {
			ChartMixed,
		},
		data() {
			return {

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
.dashboard-mixed-chart {
	height: 100%;
	.chart-tips {
		width: 100%;
		text-align: center;
	}
}
</style>