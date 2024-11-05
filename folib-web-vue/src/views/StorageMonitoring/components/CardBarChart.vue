<template>

	<a-card :bordered="false" class="dashboard-bar-chart">
		<chart-bar :height="220" :data="data"></chart-bar>
		<div class="card-title">
			<h6>{{$t('StorageMonitoring.FilesSize')}}</h6>
			<p>
				{{$t('StorageMonitoring.ThanLastUpdate')}}
				<span v-if="storageMonitoringData.filesSizeDiff !== 0 && ! storageMonitoringData.filesSizeDiff">-</span>
				<span v-else-if="storageMonitoringData.filesSizeDiff >= 0">+ {{ fileSizeConver(storageMonitoringData.filesSizeDiff) }}</span>
				<span v-else>- {{ fileSizeConver(storageMonitoringData.filesSizeDiff) }}</span>
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

	import ChartBar from '@/components/Charts/ChartBar' ;
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
						labels: ["01", "02", "03", "04", "05", "06", "07", "08", "09"],
						datasets: [{
							label: "Sales",
							backgroundColor: '#fff',
							borderWidth: 0,
							borderSkipped: false,
							borderRadius: 6,
							data: [850, 600, 500, 620, 900, 500, 900, 630, 900],
							maxBarThickness: 20,
						}, ],
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
			ChartBar,
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