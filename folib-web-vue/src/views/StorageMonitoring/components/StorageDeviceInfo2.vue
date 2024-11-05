<template>

	<a-card :bordered="false" class="header-solid storage-monitoring-storage-device"
		:bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }">
		<template #title>
			<h6 class="font-semibold m-0">{{ $t('StorageMonitoring.StorageDevice') }}</h6>
		</template>
		<!-- <template #extra>
			<a-pagination :total="50" show-less-items />
		</template> -->
		<a-list item-layout="vertical" size="large" :data-source="statsList"
			:pagination="statsList.length === 0 ? false : { pageSize: 1, total: statsList.length, showLessItems: true, showTotal:total => $t('StorageMonitoring.DeviceTotal') + ` ${total} ` + $t('StorageMonitoring.Device')}">
			<a-list-item slot="renderItem" :key="index" slot-scope="item, index">
				<a-row :gutter="[24, 24]">
					<a-col :span="24" :lg="8" :xl="8" class="stat-col mb-5" v-for="(stat, i) in item" :key="i"
						>
						<WidgetCounter :title="$t(stat.title)" :value="stat.value" :prefix="stat.prefix" :suffix="stat.suffix && stat.suffix.indexOf('.')!== -1?$t(stat.suffix):stat.suffix"
							:icon="stat.icon" :status="stat.status" :precision="stat.precision" :aIcon="stat.aIcon" :theme="stat.theme" :tag="i==0?true:false"
							:src="stat.src" class="storage-device-info"></WidgetCounter>
					</a-col>
				</a-row>
			</a-list-item>
		</a-list>
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
import WidgetCounter from '@/components/Widgets/WidgetCounter';

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
		WidgetCounter,
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
			statsList: [

			],
			nasStats: [
				{
					title: 'StorageMonitoring.StorageDeviceName',
					key: 'storageDeviceName',
					value: 0,
					prefix: "",
					suffix: "",
					aIcon: "appstore",
					theme: '',
				},
				{
					title: 'StorageMonitoring.StorageDeviceSize',
					key: 'storageDeviceSize',
					value: 0,
					prefix: "",
					suffix: "",
					precision: 2,
					aIcon: "appstore",
					theme: 'filled'
				},
				{
					title: "StorageMonitoring.StorageDeviceUsableSize",
					key: 'storageDeviceUsableSize',
					value: 0,
					suffix: "",
					precision: 2,
					src: "images/folib/storageDeviceUsableSize.svg",
					theme: '',
				},
				{
					title: "StorageMonitoring.UsedStorageDeviceSize",
					key: 'usedStorageDeviceSize',
					value: 0,
					prefix: "",
					status: "",
					suffix: "",
					precision: 2,
					src: "images/folib/usedStorageDeviceSize.svg",
					theme: '',
				},
				{
					title: "StorageMonitoring.UsedStorageDeviceSizePercentage",
					key: 'usedStorageDeviceSizePercentage',
					value: 0,
					prefix: "",
					suffix: "",
					precision: 2,
					src: "images/folib/usedStorageDeviceSizePercentage.svg",
					theme: '',
				},
				{
					title: "StorageMonitoring.ArtifactFilesSize",
					key: 'artifactsSize',
					value: 0,
					prefix: "",
					status: "",
					suffix: "",
					precision: 2,
					src: "images/folib/artifactsSize.svg",
					theme: '',
				},
				{
					title: "StorageMonitoring.UsedArtifactSizePercentage",
					key: 'usedFilesSizePercentage',
					value: 0,
					prefix: "",
					suffix: "",
					precision: 2,
					src: "images/folib/percentage.svg",
					theme: '',
				},
				{
					title: "StorageMonitoring.OtherFilesSize",
					key: 'otherFilesSize',
					value: 0,
					suffix: "",
					precision: 2,
					aIcon: "file",
					theme: 'filled'
				},
				{
					title: "StorageMonitoring.UsedOtherFilesSizePercentage",
					key: 'usedOtherFilesSizePercentage',
					value: 0,
					prefix: "",
					suffix: "",
					precision: 2,
					aIcon: "file-text",
					theme: 'filled'
				},
			],
			s3Stats: [
				{
					title: 'StorageMonitoring.StorageDeviceName',
					key: 'storageDeviceName',
					value: 0,
					prefix: "",
					suffix: "",
					aIcon: "cloud",
					theme: 'filled',
				},
				{
					title: "StorageMonitoring.UsedFilesSize",
					key: 'filesSize',
					value: 0,
					suffix: "",
					precision: 2,
					aIcon: "file",
					theme: 'filled'
				},
				{
					title: "StorageMonitoring.FilesTotal",
					key: 'filesCount',
					value: 0,
					prefix: "",
					suffix: "",
					aIcon: "file-text",
					theme: 'filled'
				},
				{
					title: "StorageMonitoring.ArtifactFilesSize",
					key: 'artifactsSize',
					value: 0,
					prefix: "",
					status: "",
					suffix: "",
					precision: 2,
					src: "images/folib/artifactsSize.svg",
					theme: '',
				},
				{
					title: "StorageMonitoring.ArtifactsTotal",
					key: 'artifactsCount',
					value: 0,
					value: 0,
					suffix: "",
					precision: 0,
					src: "images/folib/artifactCount.svg",
					theme: '',
				},	
				{
					title: "StorageMonitoring.FoldersTotal",
					key: 'foldersCount',
					value: 0,
					prefix: "",
					status: "",
					suffix: "",
					aIcon: "folder",
					theme: 'filled'
				},
				{
					title: "StorageMonitoring.ItemsTotal",
					key: 'itemsCount',
					value: 0,
					prefix: "",
					suffix: "",
					src: "images/folib/ItemCount.svg",
					theme: '',
				},
				{
					title: "StorageMonitoring.StorageCount",
					key: 'storageCount',
					value: 0,
					prefix: "",
					suffix: "",
					src: "images/folib/storageCount.svg",
					theme: '',
				},
				{
					title: "StorageMonitoring.RepositoryCount",
					key: 'repositoryCount',
					value: 0,
					prefix: "",
					suffix: "",
					src: "images/folib/repositoryCount.svg",
					theme: '',
				},
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
			this.statsList = []
			getStorageMonitoringList({ isLatest: true, dataType: 5 }).then(response => {
				this.storageDeviceInfoList = response
				for (let index in this.storageDeviceInfoList) {
					let storageDevice = this.storageDeviceInfoList[index]
					if (storageDevice.storageDeviceType === 'S3') {
						let s3Stats = JSON.parse(JSON.stringify(this.s3Stats))
						for (let stats of s3Stats) {
							stats.value = storageDevice[stats.key]
							if (stats.key.indexOf("Size") !== -1 && stats.key.indexOf("Percentage") === -1) {
								stats.value = this.fileSizeConverUnit(storageDevice[stats.key])
								stats.suffix = 'GB'
							}
							if (stats.key.indexOf("Percentage") !== -1) {
								stats.suffix = '%'
							}
							if (stats.key.indexOf("Count") !== -1) {
								stats.suffix = 'StorageMonitoring.Each'
							}
							if (stats.key.indexOf("storageDeviceName") !== -1) {
								stats.suffix = 'S3'
							}
						}
						this.statsList.push(s3Stats)
						this.getS3StorageMonitoringData(index, storageDevice.storageDeviceName)
					} else {
						let pieChartData = JSON.parse(JSON.stringify(this.pieChartData))
						pieChartData.datasets[0].data.push(this.fileSizeConverUnit(storageDevice.storageDeviceUsableSize))
						pieChartData.datasets[0].data.push(this.fileSizeConverUnit(storageDevice.filesSize))
						pieChartData.datasets[0].data.push(this.fileSizeConverUnit(storageDevice.otherFilesSize))
						storageDevice.pieChartData = pieChartData

						let nasStats = JSON.parse(JSON.stringify(this.nasStats))
						for (let stats of nasStats) {
							stats.value = storageDevice[stats.key]
							if (stats.key.indexOf("Size") !== -1 && stats.key.indexOf("Percentage") === -1) {
								stats.value = this.fileSizeConverUnit(storageDevice[stats.key])
								stats.suffix = 'GB'
							}
							if (stats.key.indexOf("Percentage") !== -1) {
								stats.suffix = '%'
							}
							if (stats.key.indexOf("Count") !== -1) {
								stats.suffix = '个'
							}
							if (stats.key.indexOf("storageDeviceName") !== -1) {
								stats.suffix = 'NAS'
							}
						}
						this.statsList.push(nasStats)
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
		border: 2px solid rgba(245, 245, 245, 0.9);
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

	.stat-col {
		height: 110px;
	}
}
</style>