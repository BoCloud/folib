<template>
	<div class="artifact-info">
		<a-drawer placement="right" width="65%" title="报告详情">
			<a-col :span="24" :md="24" class="mb-24">
				<a-card :bordered="false" class="header-solid h-full card-profile-information"
					:bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }" :headStyle="{ paddingRight: 0 }">
					<template #title>
						<h6 class="font-semibold m-0">
							<a-avatar :size="24" shape="square" :src="
								'images/folib/' + getFileType(artifactInfo ? artifactInfo.path : '') + '.svg'
							" />
							{{ artifactInfo ? artifactInfo.path : '' }}
							<div class="table-severity-info" v-if="severity.show" @click="reportVisible = true">
								<template v-if="severity.vulnerabilitesCount > 0">
									<a-tooltip>
										<template slot="title">严重</template>
										<div class="severity-info">
											<a-avatar :size="24" :src="'images/folib/critical.svg'" />
											<span class="mb-0 text-dark">{{ severity.critical }}</span>
										</div>
									</a-tooltip>

									<a-tooltip>
										<template slot="title">高危</template>
										<div class="severity-info">
											<a-avatar :size="24" :src="'images/folib/high.svg'" />
											<span class="mb-0 text-dark">{{ severity.high }}</span>
										</div>
									</a-tooltip>

									<a-tooltip>
										<template slot="title">中危</template>
										<div class="severity-info">
											<a-avatar :size="24" :src="'images/folib/medium.svg'" />
											<span class="mb-0 text-dark">{{ severity.medium }}</span>
										</div>
									</a-tooltip>

									<a-tooltip>
										<template slot="title">低危</template>
										<div class="severity-info">
											<a-avatar :size="24" :src="'images/folib/low.svg'" />
											<span class="mb-0 text-dark">{{ severity.low }}</span>
										</div>
									</a-tooltip>
								</template>
								<template v-else>
									<a-tooltip>
										<template slot="title">健康</template>
										<a-avatar :size="24" :src="'images/folib/healthy.svg'" />
									</a-tooltip>
								</template>
							</div>
						</h6>
					</template>
					<a-button type="link" slot="extra" @click="searchViewCodeHandle()">
						预览
						<a-icon :size="24" shape="square" type="eye"></a-icon>
					</a-button>
					<a class="text-dark" :href="artifactInfo ? artifactInfo.url : ''" target="_blank">{{
							artifactInfo ? artifactInfo.url : ''
					}}</a>
					<hr class="my-25" />
					<a-descriptions title="基本信息" :column="1" v-if="artifactInfo">
						<a-descriptions-item label="所属空间">
							{{ artifactInfo.storageId }}
						</a-descriptions-item>
						<a-descriptions-item label="所属仓库">
							{{ artifactInfo.repositoryId }}
						</a-descriptions-item>
						<a-descriptions-item label="名称">
							{{ artifactInfo.path }}
						</a-descriptions-item>
						<a-descriptions-item label="文件大小">
							{{ fileSizeConver(artifactInfo.sizeInBytes) }}
						</a-descriptions-item>
						<a-descriptions-item label="修改时间">
							{{ artifactInfo.lastUpdated }}
						</a-descriptions-item>
						<a-descriptions-item label="最近使用时间">
							{{ artifactInfo.lastUsed }}
						</a-descriptions-item>
						<a-descriptions-item v-if="file" label="下载次数">
							{{ artifactInfo.downloadCount }}
						</a-descriptions-item>
						<a-descriptions-item label="MD5">
							{{ artifactInfo.md5 }}
						</a-descriptions-item>
						<a-descriptions-item label="SHA-1">
							{{ artifactInfo.sha }}
						</a-descriptions-item>
					</a-descriptions>
					<hr class="my-25" />

					<a-col :span="24" v-if="artifactInfo && artifactInfo.snippets">
						<a-card :bordered="false" class="card-billing-info">
							<div class="col-info">
								<a-descriptions :title="'使用示例(' + codeParam.type + ')'" :column="1">
									<a-descriptions-item v-if="artifactInfo">
										<prism-editor class="my-editor height-300" v-if="artifactInfo"
											v-model="codeParam.code" :highlight="highlighterHandle"
											:line-numbers="false" :readonly="true"></prism-editor>
									</a-descriptions-item>
								</a-descriptions>
							</div>
							<div class="col-action">
								<a-button v-for="(item, index) in this.artifactInfo.snippets" :key="index" type="link"
									size="small" @click="changeCodeTye(item)">
									<a-avatar :size="20" shape="square"
										:src="'images/folib/' + getCodeImg(item) + '.svg'" />
								</a-button>
							</div>
						</a-card>
					</a-col>
				</a-card>
			</a-col>
			<VunlerabilityReport :report="report" :visible="reportVisible" />
		</a-drawer>
	</div>
</template>

<script>
import store from '@/store'
import { PrismEditor } from 'vue-prism-editor'
import 'vue-prism-editor/dist/prismeditor.min.css'
import { highlight, languages } from 'prismjs/components/prism-core'
import 'prismjs/components/prism-clike'
import 'prismjs/components/prism-javascript'
import 'prismjs/themes/prism-tomorrow.css'
import {
	getFileType,
	fileSizeConver,
} from '@/utils/layoutUtil'
import { getSeverity } from '@/api/folib'
import VunlerabilityReport from '@/components/Vulnerabilities/VunlerabilityReport'

export default ({
	inject: ["reload"],
	components: {
		VunlerabilityReport,
		PrismEditor,
	},
	props: {
		artifactInfo: {
			type: Object,
			default: () => { },
		},
		file: {
			type: Boolean,
			default: false,
		},
	},
	created() {
		this.artifactInfo
		this.handlerArtifactInfo()
	},
	data() {
		return {
			reportVisible: false,
			severity: {},
			report: [],
			codeParam: {},
		}
	},
	methods: {
		handlerArtifactInfo() {
			if (this.artifactInfo && this.artifactInfo.snippets) {
				this.changeCodeTye(this.artifactInfo.snippets[0])
			}
			var id = "storages/" + this.artifactInfo.storageId + "/" + this.artifactInfo.repositoryId + "/" + this.artifactInfo.path
			this.getSeverity(id)
		},
		getSeverity(id) {
			this.reportVisible = false
			var flag = id.endsWith('.sha') || id.endsWith('.sha1') || id.endsWith('.sha256') || id.endsWith('.sha512') || id.endsWith('.md5')
			if (flag) {
				return
			}
			getSeverity(id).then(res => {
				if (res.rel) {
					this.severity = res.data
					if (this.severity.report) {
						this.report = JSON.parse(this.severity.report)
					}
				}
			})
		},
		getFileType(name) {
			if (name) {
				return getFileType(name)
			}
		},
		fileSizeConver(size) {
			if (size) {
				return fileSizeConver(size)
			}
		},
		changeCodeTye(item) {
			this.codeParam = { type: item.name === 'Maven 2' ? 'maven' : item.name.toLowerCase(), code: item.code }
		},
		getCodeImg(item) {
			return item.name === 'Maven 2' ? 'maven_black' : item.name.toLowerCase()
		},
		highlighterHandle(code) {
			return highlight(code, languages.js)
		},
	}
})
</script>

<style lang="scss" scoped>
$md: 768px;

.artifact-info::v-deep {}
</style>