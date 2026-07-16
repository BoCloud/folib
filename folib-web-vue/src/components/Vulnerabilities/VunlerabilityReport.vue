<template>
    <div class="vulnerability-report">
        <a-drawer placement="right" width="55%" :title="$t('Vulnerabilities.ReportDetail')" :visible="reportVisible" @close="closeReport">
            <a-collapse default-active-key="1" :bordered="false" accordion>
                <template #expandIcon="props">
                    <a-icon type="caret-right" :rotate="props.isActive ? 90 : 0" />
                </template>
                <a-collapse-panel v-for="(item, index) in report" :key="index" style="
                background: #f7f7f7;
                border-radius: 4px;
                margin-bottom: 24px;
                border: 0;
                overflow: hidden;
              ">
                    <template slot="header">
                        <div class="collapse-panel-header-info">
                            <span class="file-name">{{ item.fileName }}</span>
                            <a-tooltip v-if="item.vulnerabilitiesCount > 0">
                                <template slot="title">{{ $t('Vulnerabilities.VulnerabilitiesNum') }}</template>
                                <a-avatar :size="24" :src="'images/folib/bug.svg'" />
                                <span class="mb-0 text-dark bug-count">{{
                                    item.vulnerabilitiesCount
                                }}</span>
                            </a-tooltip>
                            <a-tooltip v-else>
                                <template slot="title">{{ $t('Vulnerabilities.Health') }}</template>
                                <a-avatar :size="24" :src="'images/folib/healthy.svg'" />
                            </a-tooltip>
                        </div>
                    </template>
                    <a-card :bordered="false" class="card-order header-solid mb-24 mx-auto mt-20 mb-50"
                        :bodyStyle="{ paddingTop: 0 }">
                        <template #title>
                            <h6 class="mb-0">{{ item.fileName }}</h6>
                        </template>
                        <a-row :gutter="[24]" type="flex">
                            <a-col :span="24" :md="16">
                                <p class="mb-0">
                                    {{ $t('Vulnerabilities.DependencyRiskVulnerability1') }}
                                    <strong>{{ item.evidence.length }}</strong>
                                    {{ $t('Vulnerabilities.DependencyRiskVulnerability2') }}
                                    <strong>{{ item.vulnerabilitiesCount }}</strong>{{ $t('Vulnerabilities.DependencyRiskVulnerability3') }}
                                </p>
                                <p class="mb-0">
                                    MD5: <strong>{{ item.md5sum }}</strong>
                                </p>
                                <p class="mb-0">
                                    SHA256: <strong>{{ item.sha256sum }}</strong>
                                </p>
                            </a-col>
                            <a-col :span="24" :md="8" class="ml-auto text-right">
                                <p class="mb-0">
                                    {{ $t('Vulnerabilities.VersionNumber') }}: <strong>{{ item.version }}</strong>
                                </p>
                            </a-col>
                        </a-row>
                        <hr class="gradient-line" />

                        <a-row :gutter="[24]" type="flex" class="order-products" align="middle">
                            <a-col :span="24" :md="12">
                                <div class="d-flex">
                                    <a-avatar class="mr-15" :src="'images/folib/' + getImage(item.ecosystem) + '.svg'"
                                        shape="square" :size="80" />
                                    <div>
                                        <h6 class="mb-0 mt-10 font-semibold">{{ item.name }}</h6>
                                        <p class="mb-15">
                                            License: <strong>{{ item.license }}</strong>
                                        </p>
                                        <a-tag class="ant-tag-success font-semibold">{{
                                            item.ecosystem
                                        }}</a-tag>
                                    </div>
                                </div>
                            </a-col>
                            <a-col :span="24" :md="12" class="ml-auto text-right">
                                <p>{{ item.description }}</p>
                            </a-col>
                        </a-row>

                        <hr class="gradient-line" />

                        <a-row :gutter="[24]" type="flex">
                            <a-col :span="24" :md="24" :lg="24">
                                <a-table :columns="i18nVulnerColumns" :data-source="item.vulnerabilities" :pagination="false" :scroll="{ x: true }"
                                    :row-key="(r, i) => i.toString()">
                                    <a-row slot="expandedRowRender" :gutter="[24, 24]" slot-scope="record">
                                        <a-col :span="24">
                                            <a-card :bordered="false" class="card-billing-info">
                                                <div class="col-info">
                                                    <a-descriptions :title="record.references.length + $t('Vulnerabilities.references')" :column="1">
                                                        <a-descriptions-item :label="$t('Vulnerabilities.Instructions')">
                                                            {{ $t('Vulnerabilities.InformationSource') }}
                                                        </a-descriptions-item>
                                                        <a-descriptions-item :label="$t('Vulnerabilities.Links')">
                                                            <p v-for="(ritem, index1) in record.references" :key="index1">
                                                                {{ ritem.url }}
                                                            </p>
                                                        </a-descriptions-item>
                                                    </a-descriptions>
                                                </div>
                                            </a-card>
                                        </a-col>
                                    </a-row>
                                    <template slot="name" slot-scope="text, record">
                                        <div>
                                            <a>
                                                <h6 class="m-0">
                                                    {{ record.name }}
                                                </h6>
                                            </a>
                                        </div>
                                    </template>
                                    <template slot="highestSeverityText" slot-scope="highestSeverityText">
                                        <div class="table-avatar-info">
                                            <a-avatar v-if="
                                                ['CRITICAL', 'MEDIUM', 'HIGH', 'LOW'].indexOf(
                                                    highestSeverityText
                                                ) != -1
                                            " :size="24" :src="
    'images/folib/' +
    highestSeverityText.toLowerCase() +
    '.svg'
" />
                                            <a-avatar v-else shape="circle" :size="24">{{
                                                highestSeverityText.slice(0, 1)
                                            }}</a-avatar>
                                            <div class="avatar-info">
                                                <p class="mb-0 text-dark">
                                                    {{
                                                        highestSeverityText === "CRITICAL"
                                                        ? $t('Vulnerabilities.Seriously')
                                                        : highestSeverityText === "MEDIUM"
                                                            ? $t('Vulnerabilities.MediumRisk')
                                                            : highestSeverityText === "HIGH"
                                                                ? $t('Vulnerabilities.HighRisk')
                                                                : highestSeverityText === "LOW"
                                                                    ? $t('Vulnerabilities.LowRisk')
                                                                    : highestSeverityText
                                                    }}
                                                </p>
                                            </div>
                                        </div>
                                    </template>
                                    <template slot="v2_exploitabilityScore" slot-scope="text, record" v-if="record.cvssV2">{{ record.cvssV2.score
                                    }}</template>
                                    <template slot="v3_exploitabilityScore" slot-scope="text, record" v-if="record.cvssV3">{{
                                        record.cvssV3.baseScore }}</template>
                                    <template slot="versionStartIncluding" slot-scope="text, record">{{
                                        record.matchedVulnerableSoftware.versionStartIncluding
                                    }}</template>
                                    <template slot="versionEndExcluding" slot-scope="text, record">{{
                                        record.matchedVulnerableSoftware.versionEndExcluding
                                    }}</template>
                                </a-table>
                            </a-col>
                        </a-row>
                    </a-card>
                </a-collapse-panel>
            </a-collapse>
        </a-drawer>
    </div>
</template>

<script>
import {
    getLayoutType,
    getFileType,
    fileSizeConver,
    formateDate,
} from "@/utils/layoutUtil"

export default {
    props: {
        report: {
            type: Array,
            default: () => [],
        },
        reportVisible: {
            type: Boolean,
            default: false,
        },
    },
    created() { },
    data() {
        return {
            vulnerColumns: [
                {
                    title: 'CVE编号',
                    i18nKey: 'Vulnerabilities.CVENumber',
                    dataIndex: 'name',
                    scopedSlots: { customRender: 'name' },
                },
                {
                    title: '漏洞等级',
                    i18nKey: 'Vulnerabilities.VulnerabilityLevel',
                    dataIndex: 'highestSeverityText',
                    scopedSlots: { customRender: 'highestSeverityText' },
                },
                {
                    title: 'CvssV2评分',
                    i18nKey: 'Vulnerabilities.CvssV2Score',
                    dataIndex: 'cvssV2',
                    scopedSlots: { customRender: 'v2_exploitabilityScore' },
                },
                {
                    title: 'CvssV3评分',
                    i18nKey: 'Vulnerabilities.CvssV3Score',
                    dataIndex: 'cvssV3',
                    scopedSlots: { customRender: 'v3_exploitabilityScore' },
                },
                {
                    title: '引入版本',
                    i18nKey: 'Vulnerabilities.ImportVersion',
                    scopedSlots: { customRender: 'versionStartIncluding' }
                },
                {
                    title: '建议修复版本',
                    i18nKey: 'Vulnerabilities.RecommendedFixVersion',
                    scopedSlots: { customRender: 'versionEndExcluding' }
                }
            ],
        }
    },
    computed: {
      i18nVulnerColumns() {
        return this.vulnerColumns.map(column => {
          if (column.i18nKey) {
            column.title = this.$t(column.i18nKey);
          }
          return column;
        });
      }
    },
    methods: {
        closeReport() {
            this.$emit('closeReport')
        },
        getLayoutTypeHandle() {
        },
        getImage(ecosystem) {
            return ecosystem ? ecosystem : this.getLayoutTypeHandle()
        },
    },
};
</script>

<style lang="scss" scoped>
$md: 768px;

.vulnerability-report::v-deep {}
</style>
