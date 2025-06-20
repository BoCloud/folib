<template>

	<a-card :bordered="false" class="header-solid storage-monitoring-storage-device" :bodyStyle="{paddingTop: 0, paddingBottom: '16px' }">
		<template #title>
			<h6 class="font-semibold m-0">{{$t('StorageMonitoring.StorageDevice')}}</h6>
		</template>
		<a-row :gutter="[24, 24]">
			<a-col :span="24" v-for="(item, index) in storageDeviceInfoList" :key="index">
				<a-card :bordered="false" class="header-solid storage-device-info">
					<a-col :span="8">
						<a-descriptions :column="2">
							<span slot="title"> 
								<a-icon :type="item.storageDeviceType === 'S3' ? 'cloud' : 'appstore'" theme="filled"
								class="text-gray-6 text-lg" />
								<span class="ml-10">{{ item.storageDeviceName }} </span>
							</span>
							<a-descriptions-item :label="$t('StorageMonitoring.StorageDeviceName')">
								<span>{{ item.storageDeviceName }} </span>
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.StorageDeviceType')">
								{{ item.storageDeviceType }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.StorageDeviceSize')" v-if="item.storageDeviceType !== 'S3'">
								{{ fileSizeConver(item.storageDeviceSize) }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.StorageDeviceUsableSize')" v-if="item.storageDeviceType !== 'S3'">
								{{ fileSizeConver(item.storageDeviceUsableSize) }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.UsedStorageDeviceSize')" v-if="item.storageDeviceType !== 'S3'">
								{{ fileSizeConver(item.usedStorageDeviceSize) }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.UsedStorageDeviceSizePercentage')" v-if="item.storageDeviceType !== 'S3'">
								<a-tag color="red" v-if="item.usedStorageDeviceSizePercentage>=90">{{ item.usedStorageDeviceSizePercentage.toFixed(2) + '%' }}</a-tag>
								<a-tag color="orange" v-if="item.usedStorageDeviceSizePercentage>=80">{{ item.usedStorageDeviceSizePercentage.toFixed(2) + '%' }}</a-tag>
								<span v-else>{{ item.usedStorageDeviceSizePercentage + '%' }}</span>
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.ArtifactFilesSize')" v-if="item.storageDeviceType === 'S3'">
								{{ fileSizeConver(item.artifactsSize) }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.ArtifactsTotal')" v-if="item.storageDeviceType === 'S3'">
								{{ item.artifactsCount }}
							</a-descriptions-item>
							<a-descriptions-item :label="item.storageDeviceType === 'S3'?$t('StorageMonitoring.UsedFilesSize'):$t('StorageMonitoring.ArtifactFilesSize')">
								{{ fileSizeConver(item.filesSize) }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.FilesTotal')" v-if="item.storageDeviceType === 'S3'">
								{{ item.filesCount }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.FoldersTotal')" v-if="item.storageDeviceType === 'S3'">
								{{ item.foldersCount }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.ItemsTotal')" v-if="item.storageDeviceType === 'S3'">
								{{ item.itemsCount }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.UsedArtifactSizePercentage')" v-if="item.storageDeviceType !== 'S3'">
								<a-tag color="red" v-if="item.usedFilesSizePercentage>=90">{{ item.usedFilesSizePercentage.toFixed(2) + '%' }}</a-tag>
								<a-tag color="orange" v-else-if="item.usedFilesSizePercentage>=80">{{ item.usedFilesSizePercentage.toFixed(2) + '%' }}</a-tag>
								<span v-else>{{ item.usedFilesSizePercentage + '%' }}</span>
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.OtherFilesSize')" v-if="item.storageDeviceType !== 'S3'">
								{{ fileSizeConver(item.otherFilesSize) }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.UsedOtherFilesSizePercentage')" v-if="item.storageDeviceType !== 'S3'">
								<a-tag color="red" v-if="item.usedOtherFilesSizePercentage>=90">{{ item.usedOtherFilesSizePercentage.toFixed(2) + '%' }}</a-tag>
								<a-tag color="orange" v-else-if="item.usedOtherFilesSizePercentage>=80">{{ item.usedOtherFilesSizePercentage.toFixed(2) + '%' }}</a-tag>
								<span v-else>{{ item.usedOtherFilesSizePercentage + '%' }}</span>
							</a-descriptions-item>
						</a-descriptions>
					</a-col>
					<a-col :span="6">
						<ChartPie :height="180" :chartData="item.pieChartData"/>
						<div class="chart-pie-tips">{{ $t('StorageMonitoring.StorageDeviceUsageView') }} (GB)</div>
					</a-col>
					<a-col :span="5">
						<a-card class="payment-method-card storage-info">
							<img class="m-20" src="images/folib/public.svg" alt="public">
							<h6 class="card-number">
								<a-tag color="#2db7f5">
									{{ $t('StorageMonitoring.StorageCount') }} 
									{{ item.storageCount }}
      					</a-tag> 
							</h6>
						</a-card>
					</a-col>
					<a-col :span="5">
						<a-card class="payment-method-card storage-info">
							<img class="m-20" src="images/folib/repository.svg" alt="repository">
							<h6 class="card-number">
								<a-tag color="#87d068">
									{{ $t('StorageMonitoring.RepositoryCount') }} 
									{{ item.repositoryCount }}
      					</a-tag>
							</h6>
						</a-card>
					</a-col>
				</a-card>
			</a-col>
		</a-row>
	</a-card>

</template>

<script>

 import {
			getLayoutType,
			getFileType,
			fileSizeConver,
			fileSizeConverUnit,
			formateDate,
	} from "@/utils/layoutUtil"
	import ChartBar from '@/components/Charts/ChartBar';
	import ChartPie from '@/components/Charts/ChartPie';
	import ChartMixed from '@/components/Charts/ChartMixed';
	import ChartDoughnut from '@/components/Charts/ChartDoughnut';

	import {
		getStorageMonitoringList,
		getStorageMonitoringPage,
	} from "@/api/storageMonitoring";

	export default ({
		components: {
			ChartBar,
			ChartPie,
			ChartMixed,
			ChartDoughnut,
		},
		data() {
			return {
				pieChartData: {
					labels: [this.$t("StorageMonitoring.StorageDeviceUsableSize"), this.$t("StorageMonitoring.ArtifactFilesSize"), this.$t("StorageMonitoring.OtherFilesSize")],
					datasets: [{
						label: "Projects",
						weight: 9,
						cutout: 0,
						tension: 0.9,
						pointRadius: 2,
						borderWidth: 2,
						backgroundColor: ['#52C41A', '#1890FF', '#F8CAB6'],
						data: [],
						fill: false
					}],
				},
				storageDeviceInfoList: [
					
				],
			}
		},
		created() {
			this.init()
		},
		methods: {
			fileSizeConver(size) {
				if (size >= 0) {
					return fileSizeConver(size)
				}
			},
			fileSizeConverUnit(size, unit) {
				if (!unit) {
					unit = 'GB'
				}
				if (size >= 0) {
					return fileSizeConverUnit(size, unit)
				}
			},
			init() {
				this.getStorageMonitorings()
			},
			getStorageMonitorings() {
				this.storageDeviceInfoList = []
				getStorageMonitoringList({isLatest: true, dataType: 5}).then(response => {
					this.storageDeviceInfoList = response
					for (let index in this.storageDeviceInfoList) {
						let storageDevice = this.storageDeviceInfoList[index]
						if (storageDevice.storageDeviceType === 'S3') {
							this.getS3StorageMonitoringData(index, storageDevice.storageDeviceName)
						} else {
							let pieChartData = JSON.parse(JSON.stringify(this.pieChartData))
							pieChartData.datasets[0].data.push(this.fileSizeConverUnit(storageDevice.storageDeviceUsableSize))
							pieChartData.datasets[0].data.push(this.fileSizeConverUnit(storageDevice.filesSize))
							pieChartData.datasets[0].data.push(this.fileSizeConverUnit(storageDevice.otherFilesSize))
							storageDevice.pieChartData = pieChartData
						}
					}
				})
			},
			getS3StorageMonitoringData(index, storageDeviceName) {
				getStorageMonitoringPage({
					storageDeviceName: storageDeviceName,
					dataType: 3,
					isLatest: 1,
					sortField: 'filesSize',
					sortOrder: 'desc',
					limit: 5
				}).then((res) => {
					let storageDevice = this.storageDeviceInfoList[index]
					let data = res.data.rows
					let pieChartData = JSON.parse(JSON.stringify(this.pieChartData))
					pieChartData.labels = []
					pieChartData.datasets[0].backgroundColor = ['#1890FF', '#F8CAB6', '#B37FEB', '#673af3', '#52C41A']
					for (let i in data) {
						let storageData = data[i]
						pieChartData.labels.push(storageData.storageId)
						pieChartData.datasets[0].data.push(this.fileSizeConverUnit(storageData.filesSize))
						storageDevice.pieChartData = pieChartData
					}
					this.$forceUpdate()
				}).finally(() => {
				})
			},
		}
	})

</script>

<style lang="scss" scoped>
.storage-monitoring-storage-device {
	.storage-device-info {
		box-shadow: none;
		border: 2px solid #F5F5F5;
	}
	.storage-info {
		box-shadow: none;
		border: none;
	}
	.storage-info img {
		width: 32px;
		height: 32px;
	}
	.chart-pie-tips {
		text-align: center;
		font-weight: bold;
  	color: black;
	}
}
</style>