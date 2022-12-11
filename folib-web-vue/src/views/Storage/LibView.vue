<template>
  <div class="lib-view">
    <!-- Header Background Image -->
    <div class="profile-nav-bg">
      <div :class="[mouseEnter ? 'mouse-enter nested' : 'nested']"
        style="background:url(images/bg-profile.jpg) center/cover;transition:all .3s" />
      <a-row type="flex" :md="8" :xs="4">
        <search-box @mouse="searchBoxMouseStatus" @search="search" />
      </a-row>
    </div>
    <a-tabs class="tabs-sliding" :default-active-key="1" :activeKey="tabActiveKey" @change="tabChange($event)">
      <a-tab-pane :key="1" tab="仓库">
        <a-affix :offset-top="50" class="repository-affix">
          <a-row>
            <a-col :span="24" :md="24" class="mb-24">
              <!-- User Profile Card -->
              <a-card :bordered="false" class="card-profile-head" :bodyStyle="{ padding: 0 }" :targetOffset="0"
                :affix="false">
                <template #title>
                  <a-row type="flex" align="middle">
                    <a-col :span="24" :md="12" class="col-info">
                      <a>
                        <a-icon type="backward"
                          :style="{ fontSize: '32px', marginRight: '20px', opacity: '0.8', color: '#BFBFBFFF' }"
                          @click="goBack()" />
                      </a>
                      <a>
                        <a-avatar @click="createData" :size="54" shape="square"
                          :src="'images/folib/' + getLayoutTypeHandle() + '.svg'"
                          style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );" />
                      </a>
                      <div class="avatar-info">
                        <a>
                          <h4 class="font-semibold m-0" @click="createData">{{ folibRepository.id }} </h4>
                        </a>
                        <p>
                          {{ baseUrl }}api/browse/{{ folibRepository.storageId }}/{{
                              folibRepository.id
                          }}
                          <a>
                            <a-icon type="copy" @click="
                              copy(
                                baseUrl + 'api/browse/' + folibRepository.storageId + '/' + folibRepository.id
                              )
                            " />
                          </a>
                        </p>

                      </div>
                    </a-col>
                    <a-col :span="24" :md="12" style="display: flex; align-items: center; justify-content: flex-end">
                      <a v-if="folibRepository.layout === 'rpm'">
                        <small style="padding-right: 20px" @click="handleRpmUpload">
                          上传
                          <a-icon type="cloud-upload" />
                        </small>
                      </a>

                      <a v-if="folibRepository.layout === 'Raw'">
                        <small style="padding-right: 20px" @click="handleUpload">
                          上传
                          <a-icon type="cloud-upload" />
                        </small>
                      </a>
                      <a v-if="folibRepository.layout !== 'Raw'">
                        <small style="padding-right: 20px" @click="UsedHelperVisible">
                          使用帮助
                          <a-icon type="question-circle" theme="filled" />
                        </small>
                      </a>
                      <div>
                        <span class="mr-15">{{ scan.onScan ? '扫描开启' : '扫描关闭' }}</span>
                        <a-switch default-checked v-model="scan.onScan" @change="scannerChange" />
                      </div>

                    </a-col>
                  </a-row>
                </template>
              </a-card>
            </a-col>
          </a-row>
        </a-affix>
        <a-row v-if="isNotSearch === false" type="flex" :gutter="24">
          <!-- Platform Settings Column -->
          <a-col :span="24" :md="10" class="mb-24">
            <a-card :bordered="false" style="max-height:1024px;min-height:454px;overflow-y: auto" class="header-solid"
              :bodyStyle="{ paddingTop: 0, paddingBottom: 0 }">
              <template #title>
                <h6 class="font-semibold m-0">包列表</h6>
              </template>
              <a-directory-tree :replaceFields="{
                key: 'artifactPath',
                title: 'name',
                children: 'children'
              }" :tree-data="treeData" :load-data="onLoadData" @select="treeSelect">
              </a-directory-tree>
            </a-card>
          </a-col>

          <a-col :span="24" :md="14" class="mb-24">
            <a-card :bordered="false" class="header-solid h-full card-profile-information"
              :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }" :headStyle="{ paddingRight: 0 }">
              <template #title>
                <h6 v-if="folibRepository.layout !== 'Docker'" class="font-semibold m-0">
                  <a-avatar v-if="!currentTreeNode.isLeaf" :size="24" shape="square"
                    :src="'images/folib/package.svg'" />
                  <a-avatar v-if="currentTreeNode.isLeaf" :size="24" shape="square" :src="
                    'images/folib/' + getFileType(currentTreeNode.name) + '.svg'
                  " />
                  {{ currentTreeNode.name }}
                  <div class="table-severity-info" v-if="severity.show" @click="detialVisible = true">
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
                <h6 v-if="folibRepository.layout === 'Docker'" class="font-semibold m-0">
                  <a-avatar :size="24" shape="square" :src="'images/folib/docker-s.svg'" />
                  {{ currentTreeNode.name }}
                </h6>
              </template>

              <a-button v-if="currentFileDetial && folibRepository.layout !== 'Raw'" type="link" slot="extra" @click="viewCodeHandle()">
                {{ currentFileDetial.listTree ? '包' : viewCodes ? '文件' : folibRepository.layout === 'Docker' ? '详情' : ''
                }}预览
                <a-icon :size="24" shape="square" type="eye"></a-icon>
              </a-button>
              <a v-if="currentTreeNode.url && folibRepository.layout !== 'Docker'" class="text-dark"
                :href="currentTreeNode.url.search('http://localhost:38080/') !== -1 ? currentTreeNode.url.replace('http://localhost:38080/', baseUrl) : currentTreeNode.url"
                target="_blank">{{ currentTreeNode.url.search('http://localhost:38080/') !==
                    -1 ? currentTreeNode.url.replace('http://localhost:38080/', baseUrl) : currentTreeNode.url
                }}</a>


              <!-- <a-popconfirm placement="topRight" ok-text="删除" cancel-text="取消" okType="danger"
                @confirm="deletePackageHandle">
                <template slot="title">
                  <p>确定删除么？</p>
                  <p>不要冲动，再好好想想</p>
                </template>
                <a-icon slot="icon" type="question-circle-o" style="color: red" />
                <a-button v-if="currentTreeNode.name" type="link">
                  删除
                  <a-icon :size="24" shape="square" type="delete"></a-icon>
                </a-button>
              </a-popconfirm> -->
              <a-dropdown v-if="currentTreeNode.url">
                <a-menu slot="overlay" @click="handleMenuClick">
                  <a-menu-item key="1" v-if="folibRepository.type === 'hosted'">
                    <a-icon type="copy" />复制
                  </a-menu-item>
                  <a-menu-item key="2" v-if="folibRepository.type === 'hosted'">
                    <a-icon type="swap" />移动
                  </a-menu-item>
                  <a-menu-item key="3">
                    <a-icon type="delete" />删除
                  </a-menu-item>
                </a-menu>
                <a-button style="margin-left: 8px">操作
                  <a-icon type="down" />
                </a-button>
              </a-dropdown>
              <hr class="my-25" />
              <a-descriptions v-if="folibRepository.layout !== 'Docker'" title="基本信息" :column="1">
                <a-descriptions-item label="所属空间">
                  {{ currentTreeNode.storageId }}
                </a-descriptions-item>
                <a-descriptions-item label="所属仓库">
                  {{ currentTreeNode.repositoryId }}
                </a-descriptions-item>
                <a-descriptions-item label="名称">
                  {{ currentTreeNode.name }}
                </a-descriptions-item>
                <a-descriptions-item label="路径">
                  {{ currentTreeNode.artifactPath }}
                </a-descriptions-item>
                <a-descriptions-item label="文件大小">
                  {{ fileSizeConver(currentTreeNode.size) }}
                </a-descriptions-item>
                <a-descriptions-item label="修改时间">
                  {{ formateDate(currentTreeNode.lastModified) }}
                </a-descriptions-item>
                <a-descriptions-item v-if="currentFileDetial" label="最近使用时间">
                  {{ currentFileDetial.lastUsedTime }}
                </a-descriptions-item>
                <a-descriptions-item v-if="currentFileDetial" label="下载次数">
                  {{ currentFileDetial.downloadCount }}
                </a-descriptions-item>
                <a-descriptions-item v-if="currentFileDetial" label="MD5">
                  {{ currentFileDetial.md5 }}
                </a-descriptions-item>
                <a-descriptions-item v-if="currentFileDetial" label="SHA-1">
                  {{ currentFileDetial.sha }}
                </a-descriptions-item>
              </a-descriptions>
              <a-descriptions v-if="folibRepository.layout === 'Docker'" title="基本信息" :column="1">
                <a-descriptions-item label="所属空间">
                  {{ currentTreeNode.storageId }}
                </a-descriptions-item>
                <a-descriptions-item label="所属仓库">
                  {{ currentTreeNode.repositoryId }}
                </a-descriptions-item>
                <a-descriptions-item v-if="currentFileDetial" label="镜像名称">
                  {{ currentFileDetial.imageName }}
                </a-descriptions-item>
                <a-descriptions-item :label="currentFileDetial ? '版本号' : '名称'">
                  {{ currentTreeNode.name }}
                </a-descriptions-item>
                <a-descriptions-item v-if="currentFileDetial" label="文件大小">
                  {{ fileSizeConver(currentFileDetial.size) }}
                </a-descriptions-item>
                <a-descriptions-item v-if="currentFileDetial" label="SHA-256">
                  {{ currentFileDetial.sha256 }}
                </a-descriptions-item>
                <a-descriptions-item v-if="currentFileDetial" label="修改时间">
                  {{ currentFileDetial.lastModified }}
                </a-descriptions-item>
                <a-descriptions-item v-if="currentFileDetial" label="层数">
                  {{ currentFileDetial.manifest.layers.length }}
                </a-descriptions-item>
                <a-descriptions-item v-if="currentFileDetial" label="制作Docker版本">
                  {{ currentFileDetial.manifestConfig.docker_version }}
                </a-descriptions-item>
                <a-descriptions-item v-if="currentFileDetial" label="镜像OS">
                  <a-tag> {{ currentFileDetial.manifestConfig.os }}</a-tag>

                </a-descriptions-item>
                <a-descriptions-item v-if="currentFileDetial" label="基础架构">
                  {{ currentFileDetial.manifestConfig.architecture }}
                </a-descriptions-item>
              </a-descriptions>
              <hr class="my-25" />

              <a-col :span="24"
                v-if="currentFileDetial && currentFileDetial.snippets && currentFileDetial.snippets.length > 0">
                <a-card :bordered="false" class="card-billing-info">
                  <div class="col-info">
                    <a-descriptions :title="'使用示例(' + codeParam.type + ')'" :column="1">
                      <a-descriptions-item v-if="currentFileDetial">
                        <prism-editor class="my-editor height-300" v-if="currentFileDetial" v-model="codeParam.code"
                          :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
                      </a-descriptions-item>
                    </a-descriptions>
                  </div>
                  <div class="col-action">
                    <a-button v-for="(item, index) in this.currentFileDetial.snippets" :key="index" type="link"
                      size="small" @click="changeCodeTye(item)">
                      <a-avatar :size="20" shape="square" :src="'images/folib/' + getCodeImg(item) + '.svg'" />
                    </a-button>
                  </div>
                </a-card>
              </a-col>
            </a-card>
          </a-col>
        </a-row>
        <a-row v-if="isNotSearch === true" type="flex" :gutter="24">
          <!-- Platform Settings Column -->
          <a-col :span="24" :md="24" class="mb-24">
            <a-card :bordered="false" style="max-height:1024px;min-height:454px;overflow-y: auto" class="header-solid"
              :bodyStyle="{ paddingTop: 0, paddingBottom: 0 }">
              <div class="mx-25">
                <a-row type="flex" :gutter="24">
                  <a-col :span="24" md="12">
                    <label for="" class="ml-10">显示数量</label>
                    <a-select class="ml-10 mt-10" v-model="artifactQuery.limit" @change="onPageSizeChange"
                      style="width: 70px">
                      <a-select-option :value="5">5</a-select-option>
                      <a-select-option :value="10">10</a-select-option>
                      <a-select-option :value="15">15</a-select-option>
                      <a-select-option :value="20">20</a-select-option>
                      <a-select-option :value="25">25</a-select-option>
                    </a-select>
                    <a-config-provider class="ml-10 mt-10" :locale="locale" style="width: 290px">
                      <a-range-picker :show-time="{ placeholder: '选择时间', format: 'HH:mm' }" format="YYYY-MM-DD HH:mm"
                        :placeholder="['开始日期', '结束日期']" @change="dateChange" @ok="dateConfirm" />
                    </a-config-provider>
                  </a-col>
                  <a-col :span="24" md="12">
                  </a-col>
                </a-row>
              </div>
              <template #title>
                <h6 class="font-semibold m-0">搜索列表</h6>
              </template>

              <a-table class="mt-20" :columns="columns" :data-source="searchData" @change="handleTableChange"
                :pagination="{ pageSize: artifactQuery.limit, current: artifactQuery.page, total: artifactQuery.total, showLessItems: true }">
                <template slot="path" slot-scope="text, record">
                  <a>
                    <div class="table-avatar-info" @click="searchDataHandle(record)">
                      <a-avatar shape="circle" :size="24" :src="'images/folib/' + getFileType(record.path) + '.svg'" />
                      <div class="avatar-info search-column-path">
                        <p class="mb-0 text-dark">
                          {{ record.artifactPath }}
                        </p>
                      </div>
                    </div>
                  </a>
                </template>
                <template slot="sizeInBytes" slot-scope="sizeInBytes">{{ fileSizeConver(sizeInBytes) }}</template>
              </a-table>
            </a-card>
          </a-col>
        </a-row>
      </a-tab-pane>
      <a-tab-pane :key="2" tab="安全">
        <a-row v-if="tabActiveKey == 2" type="flex" :gutter="24">
          <!-- <div class="profile-nav-bg statistics-bg">
        <div class="nested" style="background:url(images/bg-profile.jpg) center/cover;transition:all .3s" />
      </div> -->
          <a-col :span="24" :xl="4" class="mb-24 statistics">
            <a-card :bordered="false" class="widget-2">
              <a-statistic :value="vulnerabilityStatistics.artifactCount">
                <template #title>
                  <div class="icon">
                    <a-icon type="appstore" theme="filled" :style="{ fontSize: '28px' }" />
                  </div>
                  <h6>制品</h6>
                  <p>制品总数（个）</p>
                </template>
              </a-statistic>
            </a-card>
          </a-col>
          <a-col :span="24" :xl="4" class="mb-24 statistics">
            <a-card :bordered="false" class="widget-2">
              <a-statistic :value="vulnerabilityStatistics.downloadCount">
                <template #title>
                  <div class="icon">
                    <a-icon type="cloud-download" :style="{ fontSize: '28px' }" />
                  </div>
                  <h6>下载</h6>
                  <p>下载总数（次）</p>
                </template>
              </a-statistic>
            </a-card>
          </a-col>
          <a-col :span="24" :xl="4" class="mb-24 statistics">
            <a-card :bordered="false" class="widget-2">
              <a-statistic :value="vulnerabilityStatistics.dependencyCount">
                <template #title>
                  <div class="icon">
                    <a-icon type="control" theme="filled" :style="{ fontSize: '28px' }" />
                  </div>
                  <h6>依赖</h6>
                  <p>依赖总数</p>
                </template>
              </a-statistic>
            </a-card>
          </a-col>
          <a-col :span="24" :xl="4" class="mb-24 statistics">
            <a-card :bordered="false" class="widget-2">
              <a-statistic :value="vulnerabilityStatistics.vulnerabilityCount">
                <template #title>
                  <div class="icon">
                    <a-icon type="bug" theme="filled" :style="{ fontSize: '28px' }" />
                  </div>
                  <h6>漏洞</h6>
                  <p>漏洞总数</p>
                </template>
              </a-statistic>
            </a-card>
          </a-col>
          <a-col :span="24" :xl="4" class="mb-24 statistics">
            <a-card :bordered="false" class="widget-2 vulnerability-count" @click="vulnerabilityDrawerShow(1)">
              <a-statistic :value="vulnerabilityStatistics.whiteCount"
                :value-style="{ color: 'green', 'text-decoration': 'underline' }">
                <template #title>
                  <div class="icon">
                    <a-icon type="file-done" :style="{ fontSize: '28px' }" />
                  </div>
                  <h6>白名单</h6>
                  <p>漏洞白名单</p>
                </template>
              </a-statistic>
            </a-card>
          </a-col>
          <a-col :span="24" :xl="4" class="mb-24 statistics">
            <a-card :bordered="false" class="widget-2 vulnerability-count" @click="vulnerabilityDrawerShow(2)">
              <a-statistic :value="vulnerabilityStatistics.blackCount"
                :value-style="{ color: '#cf1322', 'text-decoration': 'underline' }">
                <template #title>
                  <div class="icon">
                    <a-icon type="exception" :style="{ fontSize: '28px' }" />
                  </div>
                  <h6>黑名单</h6>
                  <p>漏洞黑名单</p>
                </template>
              </a-statistic>
            </a-card>
          </a-col>
        </a-row>
        <a-card>
          <Vulnerability :vulnerabilityColumns="vulnerabilityColumns" :queryStorageId="false" :vulnerabilityLevel="2"
            :queryRepositoryId="false" :storageId="folibRepository.storageId" :repositoryId="folibRepository.id"
            ref="vulnerability">
          </Vulnerability>
        </a-card>
      </a-tab-pane>
    </a-tabs>
    <!-- / Header Background Image -->

    <!-- User Profile Card -->

    <a-drawer placement="right" width="65%" title="制品详情" :visible="artifactVisible" @close="artifactVisible = false">
      <a-card :bordered="false" class="header-solid h-full card-profile-information"
        :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }" :headStyle="{ paddingRight: 0 }">
        <template #title>
          <h6 class="font-semibold m-0">
            <a-avatar :size="24" shape="square" :src="
              'images/folib/' + getFileType(searchDataCurrentSelect ? searchDataCurrentSelect.path : '') + '.svg'
            " />
            {{ searchDataCurrentSelect ? searchDataCurrentSelect.path : '' }}
            <div class="table-severity-info" v-if="severity.show" @click="detialVisible = true">
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
        <a class="text-dark" :href="searchDataCurrentSelect ? searchDataCurrentSelect.url : ''" target="_blank">{{
            searchDataCurrentSelect ? searchDataCurrentSelect.url : ''
        }}</a>
        <hr class="my-25" />
        <a-descriptions title="基本信息" :column="1" v-if="searchDataCurrentSelect">
          <a-descriptions-item label="所属空间">
            {{ searchDataCurrentSelect.storageId }}
          </a-descriptions-item>
          <a-descriptions-item label="所属仓库">
            {{ searchDataCurrentSelect.repositoryId }}
          </a-descriptions-item>
          <a-descriptions-item label="名称">
            {{ searchDataCurrentSelect.path }}
          </a-descriptions-item>
          <a-descriptions-item label="文件大小">
            {{ fileSizeConver(searchDataCurrentSelect.sizeInBytes) }}
          </a-descriptions-item>
          <a-descriptions-item label="修改时间">
            {{ searchDataCurrentSelect.lastUpdated }}
          </a-descriptions-item>
          <a-descriptions-item label="最近使用时间">
            {{ searchDataCurrentSelect.lastUsed }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial" label="下载次数">
            {{ searchDataCurrentSelect.downloadCount }}
          </a-descriptions-item>
          <a-descriptions-item label="MD5">
            {{ searchDataCurrentSelect.md5 }}
          </a-descriptions-item>
          <a-descriptions-item label="SHA-1">
            {{ searchDataCurrentSelect.sha }}
          </a-descriptions-item>
        </a-descriptions>
        <hr class="my-25" />

        <a-col :span="24"
          v-if="searchDataCurrentSelect">
          <a-card :bordered="false" class="card-billing-info">
            <div class="col-info">
              <a-descriptions :title="'使用示例(' + codeParam.type + ')'" :column="1">
                <a-descriptions-item v-if="searchDataCurrentSelect">
                  <prism-editor class="my-editor height-300" v-if="searchDataCurrentSelect" v-model="codeParam.code"
                    :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
                </a-descriptions-item>
              </a-descriptions>
            </div>
            <div class="col-action">
              <a-button v-for="(item, index) in this.searchDataCurrentSelect.snippets" :key="index" type="link"
                size="small" @click="changeCodeTye(item)">
                <a-avatar :size="20" shape="square" :src="'images/folib/' + getCodeImg(item) + '.svg'" />
              </a-button>
            </div>
          </a-card>
        </a-col>
      </a-card>
    </a-drawer>

    <a-drawer placement="right" width="45%" :title="currentTreeNode.name" :visible="viewCodeVisible"
      @close="closeViewCodeDialog">
      <div class="mx-auto m-50">
        <div class="mb-50">
          <a-card :bordered="false" class="header-solid">
            <a-directory-tree v-if="currentFileDetial && currentFileDetial.listTree"
              :replaceFields="{ title: 'name', children: 'children' }" :tree-data="currentFileDetial.listTree" />
          </a-card>
          <prism-editor class="my-editor height-300"
            v-if="currentFileDetial && viewCodes && folibRepository.layout !== 'Docker'" v-model="viewCodes"
            :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>

          <a-tabs v-if="currentFileDetial && currentManifest && folibRepository.layout === 'Docker'"
            class="tabs-sliding" default-active-key="1">
            <a-tab-pane key="1" tab="Layers">
              <a-timeline>
                <a-timeline-item color="primary" v-for="(key, index) in currentManifest.config" :key="index"
                  v-if="currentManifest.config[index]">
                  {{ index }}
                  <p>
                    {{ currentManifest.config[index] }}
                  </p>
                </a-timeline-item>
              </a-timeline>
            </a-tab-pane>
            <a-tab-pane key="2" tab="制作历史">
              <a-timeline>
                <a-timeline-item color="primary" v-for="(key, index) in currentManifest.history" :key="index"
                  v-if="currentManifest.history[index]">
                  {{ formateDate(key.created) }}
                  <p>
                    {{ key.created_by }}
                  </p>
                </a-timeline-item>
              </a-timeline>
            </a-tab-pane>
          </a-tabs>


        </div>
      </div>
    </a-drawer>
    <a-drawer placement="right" width="45%" v-if="searchDataCurrentSelect" :title="searchDataCurrentSelect.path"
      :visible="searchViewCodeVisible" @close="closeSearchviewCodeDialog">
      <div class="mx-auto m-50" style="max-width: 1000px;">
        <div class="mb-50">
          <a-card :bordered="false" class="header-solid">
            <a-directory-tree v-if="searchDataCurrentSelect && searchDataCurrentSelect.treeNode"
              :replaceFields="{ title: 'name', children: 'children' }" :tree-data="searchDataCurrentSelect.treeNode" />
          </a-card>
          <prism-editor class="my-editor height-300" v-if="searchDataCurrentSelect && searchViewCodes"
            v-model="searchViewCodes" :highlight="highlighterHandle" :line-numbers="false" :readonly="true">
          </prism-editor>
        </div>
      </div>
    </a-drawer>
    <a-drawer placement="right" width="65%" title="使用说明" :visible="usedVisible" @close="closeUsedVisibleDialog">
      <a-timeline v-if="repositoryType === 'maven'">
        <a-timeline-item color="primary">
          Maven全局配置
          <small>maven settings配置</small>
          <p>
            你需要复制以下配置到你的maven的/conf/settings.xml中
          </p>

          <prism-editor class="my-editor height-300" :value="'<mirror>\n' +
          '   <id>' + folibRepository.id + '</id>\n' +
          '   <name>' + folibRepository.id + '</name>\n' +
          '   <url>' + baseUrl + 'storages/' + folibRepository.storageId + '/' + folibRepository.id + '</url>\n' +
          '   <mirrorOf>*</mirrorOf>\n' +
          '</mirror>'" :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          POM配置
          <small>pom.xml配置</small>
          <p>
            通常需要在pom.xml中进行指定上传的配置，和常用的maven仓库一样使用,具体pom配置可参阅：https://maven.apache.org/pom.html
          </p>
          <p>
            注意：本仓库类型为:<strong>{{ folibRepository.type === 'proxy' ? '代理库' : folibRepository.type === 'group' ? '组合库' :
                '本地库'
            }}</strong>{{ folibRepository.type === 'proxy' ? '不支持上传' : folibRepository.type === 'group' ?
    '不支持上传' : '可以上传'
}}
          </p>
          <prism-editor class="my-editor height-300" :value="'<repositories>\n' +
          '   <repository>\n' +
          '      <id>' + folibRepository.id + '</id>\n' +
          '      <url>' + baseUrl + 'storages/' + folibRepository.storageId + '/' + folibRepository.id + '</url>\n' +
          '   </repository>\n' +
          '</repositories>\n' +
          '\n' +
          '<distributionManagement>\n' +
          '   <repository>\n' +
          '      <id>' + folibRepository.id + '</id>\n' +
          '      <url>' + baseUrl + 'storages/' + folibRepository.storageId + '/' + folibRepository.id + '</url>\n' +
          '   </repository>\n' +
          '</distributionManagement>'" :highlight="highlighterHandle" :line-numbers="false" :readonly="true">
          </prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          命令操作
          <small>maven 通常使用命令</small>
          <p>
            和通常maven一样使用，具体参阅：https://maven.apache.org/index.html
          </p>

          <prism-editor class="my-editor height-300" :value="'mvn clean intall\n' +
          'mvn clean deploy'" :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'gradle'">
        <a-timeline-item color="primary">
          Gradle配置
          <small>Gradle配置仓库</small>
          <p>
            你需要在 build.gradle 文件中加入以下代码:
          </p>

          <prism-editor class="my-editor height-300" :value="'allprojects {\n' +
          '  repositories {\n' +
          '    maven {\n' +
          '      url \'' + baseUrl + 'storages/' + folibRepository.storageId + '/' + folibRepository.id + '\'\n' +
          '    }\n' +
          '    mavenLocal()\n' +
          '    mavenCentral()\n' +
          '  }\n' +
          '}'" :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          命令操作
          <small>Gradle 通常使用命令</small>
          <p>
            和通常Gradle一样使用，具体参阅：https://docs.gradle.org/current/userguide/userguide.html
          </p>

          <prism-editor class="my-editor height-300" :value="'gradle dependencies \n' +
          './gradlew dependencies '" :highlight="highlighterHandle" :line-numbers="false" :readonly="true">
          </prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'sbt'">
        <a-timeline-item color="primary">
          SBT配置
          <small>SBT配置仓库</small>
          <p>
            你需要编辑或新建 ${HOME}/.sbt/repositories，文件中加入以下代码:
          </p>

          <prism-editor class="my-editor height-300"
            :value="'[repositories]\n' +
            'local\n' +
            '' + folibRepository.id + ': ' + baseUrl + 'storages/' + folibRepository.storageId + '/' + folibRepository.id + ''" :highlight="highlighterHandle"
            :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          全局配置
          <small>SBT全局配置</small>
          <p>
            编辑 ${sbt_安装目录}/conf/sbtconfig.txt，如果你使用的 idea，在 settings->SBT-> jvm parameters 添加
          </p>

          <prism-editor class="my-editor height-300"
            :value="'-Dsbt.override.build.repos=true ## 忽略工程自定义的 resolvers，采用全局配置\n'" :highlight="highlighterHandle"
            :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          使用命令操作
          <small>SBT命令使用</small>
          <p>
            和通常SBT命令一样使用，具体参阅：https://www.scala-sbt.org/
          </p>

          <prism-editor class="my-editor height-300" :value="'sbt compile publish'" :highlight="highlighterHandle"
            :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'ivy'">
        <a-timeline-item color="primary">
          ivy配置
          <small>ivy配置仓库</small>
          <p>
            你需要修改 ${USER_HOME}/.ivy2/ivysettings.xml，文件中加入以下代码:
          </p>

          <prism-editor class="my-editor height-300" :value="ivyCode" :highlight="highlighterHandle"
            :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          使用命令操作
          <small>ant-ivy命令使用</small>
          <p>
            和通常SBT命令一样使用，具体参阅：https://ant.apache.org/ivy/history/2.4.0/use/makepom.html
          </p>

          <prism-editor class="my-editor height-300" :value="'ant build deploy'" :highlight="highlighterHandle"
            :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'npm'">
        <a-timeline-item color="primary">
          NPM全局配置
          <small>NPM配置全局配置</small>
          <p>
            你可以全局配置npm的mirror,操作如下:
          </p>

          <prism-editor class="my-editor height-300" :value="'npm config set registry ' + baseUrl + 'storages/' + folibRepository.storageId + '/' + folibRepository.id + '\n' +
          '\n' +
          'npm config list #查看npm当前配置'" :highlight="highlighterHandle" :line-numbers="false" :readonly="true">
          </prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          工程配置
          <small>该方式只对当前工程生效</small>
          <p>
            需要在仓库下创建.npmrc文件并填入如下：
          </p>

          <prism-editor class="my-editor height-300" :value="'registry=' + baseUrl + 'storages/' + folibRepository.storageId + '/' + folibRepository.id + '\n' +
          'always-auth=true\n' +
          'email=yours4@example.com\n' +
          '_auth=YWRtaW46cGFzc3dvcmQ=\n' +
          '\n' +
          '; `_auth` 是 base64 的token\n' +
          '; 你也可以采用用户名密码模式:\n' +
          '; username=admin\n' +
          '; _password=password'" :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          命令操作
          <small>NPM 通常使用命令</small>
          <p>
            和通常NPM一样使用，具体参阅：https://npmjs.org/
          </p>

          <prism-editor class="my-editor height-300" :value="'npm install   #安装依赖\n' +
          '\n' +
          'npm publish  #上传依赖'" :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'rpm'">
        <a-timeline-item color="primary">
          RPM配置
          <small>centOS yum源配置</small>
          <p>
            在/etc/yum.repos.d/中添加一个local_test.repo文件,镜像服务器为阿里云,操作如下:
          </p>

          <prism-editor class="my-editor height-300" :value="
          '[local_test]' + '\n' +
          'name=CentOS-$releasever - Base - mirrors.aliyun.com' + '\n' +
          'enabled=1' + '\n' +
          'baseurl=' + baseUrl + 'storages/' + folibRepository.storageId + '/' + folibRepository.id + '/'  + '\n' +
          'gpgcheck=0'" :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          命令操作
          <small>yum 使用命令</small>
          <p>
            仅供参考，详情请查相关文档
          </p>

          <prism-editor class="my-editor height-300" :value="
          'yum clean all #清除YUM缓存' + '\n' +
          'yum repolist #显示所有仓库' + '\n' +
          'yum install --downloadonly --downloaddir=/folib_test/mysql mysql #拉mysql 相关rpm包到/folib_test/mysql 目录下'"
            :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'helm'">
        <a-timeline-item color="primary">
          Helm配置
          <p>
            将folib helm仓添加到本地操作步骤
          </p>

          <prism-editor class="my-editor height-300" :value="
          'helm  registry  login  '+ baseUrl +folibRepository.storageId + '/' + folibRepository.id+
          '\n' +
          '\n' +
          'helm  repo  add   '+ folibRepository.id +'   ' + baseUrl + folibRepository.storageId + '/' + folibRepository.id+
          '\n'
          " :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          上传Chart包到Helm仓库
          <p>
            安装 helm-cm-push插件
          </p>
          <prism-editor class="my-editor height-300" :value="
          '1.   https://github.com/chartmuseum/helm-push/releases 下载各个系统下的 helm-cm-push 安装包'+ '\n' +
          '2.   把安装包复制到 helm 的plugins目录下解压     ' + '\n' + '\n' +
           '\n' +
          '\n' +
           'helm-cm-push 命令上传'    +'\n' + '\n' +
          '1. 进入 helm-cm-push plugins 插件bin目录       #helm env 查看plugins目录位置'+ '\n' +
          '\n' +
          '2.   执行上传'+'\n' +
          '例如 ：上传/app/fluentd-4.5.2.tgz 的chart包 到'+ folibRepository.id+'\n' +'\n' +
          './helm-cm-push  /app/fluentd-4.5.2.tgz  '+ folibRepository.id+'\n'+
           '\n' +
          '参数说明：第一个参数是cahrt 包全路径   第二个参数是加入到本地的helm 仓库名. --username  --password 可选鉴权使用' + '\n'"
                        :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>

        <a-timeline-item color="primary">
          helm 使用常用命令
          <p>
            详细使用参考官网 https://helm.sh/zh/docs/intro/using_helm/
          </p>
          <prism-editor class="my-editor height-300" :value="
          'helm reop update  #更新本地仓库'+ '\n' +
           '\n' +
          'helm search repo mysql     #搜索本地的mysql charts' + '\n' +
           '\n' +
          'helm pull  '+ folibRepository.id+'/mysql   ./    #将最新的mysql 下载到本地  --version 可指定版本' + '\n' "
                        :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'yarn'">
        <a-timeline-item color="primary">
          Yarn配置
          <small>Yarn配置全局配置</small>
          <p>
            你可以全局配置Yarn的mirror,操作如下:
          </p>

          <prism-editor class="my-editor height-300" :value="'yarn config set registry ' + baseUrl + 'storages/' + folibRepository.storageId + '/' + folibRepository.id + '\n' +
          '\n' +
          'yarn config get registry #查看npm当前配置'" :highlight="highlighterHandle" :line-numbers="false" :readonly="true">
          </prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          工程配置
          <small>该方式只对当前工程生效</small>
          <p>
            需要在仓库下创建.npmrc文件并填入如下：
          </p>

          <prism-editor class="my-editor height-300" :value="'registry=' + baseUrl + 'storages/' + folibRepository.storageId + '/' + folibRepository.id + '\n' +
          'always-auth=true\n' +
          'email=yours4@example.com\n' +
          '_auth=YWRtaW46cGFzc3dvcmQ=\n' +
          '\n' +
          '; `_auth` 是 base64 的token\n' +
          '; 你也可以采用用户名密码模式:\n' +
          '; username=admin\n' +
          '; _password=password'" :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          命令操作
          <small>Yarn 通常使用命令</small>
          <p>
            和通常Yarn一样使用，具体参阅：https://npmjs.org/
          </p>

          <prism-editor class="my-editor height-300" :value="'yarn install   #安装依赖\n' +
          '\n' +
          'yarn publish  #上传依赖'" :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'pypi'">
        <a-timeline-item color="primary">
          Pypi配置
          <small>Pypi配置</small>
          <p>
            编写.pypirc配置文件如下:
          </p>

          <prism-editor class="my-editor height-300" :value="'[distutils]\n' +
          'index-servers =' + baseUrl + 'storages/' + folibRepository.storageId + '/' + folibRepository.id + '\n' +
          'pypi\n' +
          'local\n' +
          '\n' +
          '[pypi]\n' +
          'username:你的用户名\n' +
          'password:你的密码\n' +
          '\n' +
          '[local]\n' +
          'repository:' + baseUrl + 'storages/' + folibRepository.storageId + '/' + folibRepository.id + '\n' +
          'username: 你的用户名\n' +
          'password: 你的密码'" :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          打包上传
          <small>该方式打包时指定仓库</small>
          <p>
            如下命令：
          </p>

          <prism-editor class="my-editor height-300" :value="'python3 -m twine upload --username admin --password password --repository-url ' + baseUrl + 'storages/' + folibRepository.storageId + '/' + folibRepository.id + '\n' +
          '\n' +
          'pypi-releases  dist/* --verbose'" :highlight="highlighterHandle" :line-numbers="false" :readonly="true">
          </prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          命令操作
          <small>Pypi 通常使用命令</small>
          <p>
            操作命令和通常Pypi一样使用，具体参阅：https://pypi.org/
          </p>

          <prism-editor class="my-editor height-300" :value="'python3 setup.py sdist bdist_wheel'"
            :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'docker'">
        <a-timeline-item color="primary">
          Ubuntu配置
          <small>针对Docker客户端版本大于 1.10.0 的用户</small>
          <p>
            您可以通过修改daemon配置文件/etc/docker/daemon.json来使用:
          </p>
          <prism-editor class="my-editor height-300" :value="dockerCode.ubuntu" :highlight="highlighterHandle"
            :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          CentOS配置
          <small>针对Docker客户端版本大于 1.10.0 的用户</small>
          <p>
            您可以通过修改daemon配置文件/etc/docker/daemon.json来使用:
          </p>
          <prism-editor class="my-editor height-300" :value="dockerCode.centos" :highlight="highlighterHandle"
            :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          MacOS配置
          <small>针对安装了Docker for Mac的用户，您可以参考以下配置步骤：</small>
          <p>
            在任务栏点击 Docker Desktop 应用图标 -> Perferences，在左侧导航菜单选择 Docker Engine，在右侧输入栏编辑 json 文件。将:
          </p>
          <prism-editor class="my-editor height-300" :value="dockerCode.macos" :highlight="highlighterHandle"
            :line-numbers="false" :readonly="true"></prism-editor>
          <p>
            加到"insecure-registries"的数组里，点击 Apply & Restart按钮，等待Docker重启
          </p>
        </a-timeline-item>
        <a-timeline-item color="primary">
          Windows配置
          <small>针对安装了Docker for Windows的用户，您可以参考以下配置步骤：</small>
          <p>
            在系统右下角托盘图标内右键菜单选择 Settings，打开配置窗口后左侧导航菜单选择 Docker Daemon。编辑窗口内的JSON串，填写下方地址：
          </p>
          <prism-editor class="my-editor height-300" :value="dockerCode.windows" :highlight="highlighterHandle"
            :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>

        <a-timeline-item color="primary">
          镜像打包命名说明
          <small>请一定要看，这决定了你的镜像包能否上传：</small>
          <p>
            镜像命名规则如下：仓库访问url/存储空间/仓库名称/镜像名称:版本号，具体如下：
          </p>
          <prism-editor class="my-editor height-300"
            :value="'docker build -t ' + baseUrl.replace('http://', '') + folibRepository.storageId + '/' + folibRepository.id + '/demo:latest .'"
            :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'nuget'">
        <a-timeline-item color="primary">
          NuGet+Mono配置
          <small>添加默认推送存储库 URL</small>
          <p>
            示例如下，详细请看文档
          </p>
          <prism-editor class="my-editor height-300"
            :value="'$ mono --runtime=v4.0 nuget.exe config -set DefaultPushSource=' + baseUrl + 'storages/' + folibRepository.storageId + '/' + folibRepository.id + ' -ConfigFile ./.nuget/NuGet.config'"
            :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          Nuget+Visual Studio配置
          <small>以下为示例</small>
          <p>
            为了方便访问folib可将 -Source 选项附加到 NuGet.exe：
          </p>
          <prism-editor class="my-editor height-300"
            :value="'nuget <command> -Source ' + baseUrl + 'storages/' + folibRepository.storageId + '/' + folibRepository.id + ''"
            :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
          <p>
            Visual Studio中的详细配置请看平台帮助文档
          </p>
        </a-timeline-item>
      </a-timeline>
    </a-drawer>

    <a-drawer placement="right" width="65%" title="报告详情" :visible="detialVisible" @close="closeDialog">
      <a-collapse default-active-key="1" :bordered="false" accordion>
        <template #expandIcon="props">
          <a-icon type="caret-right" :rotate="props.isActive ? 90 : 0" />
        </template>
        <a-collapse-panel v-for="(item, index) in currentReport" :key="index"
          style='background: #f7f7f7;border-radius: 4px;margin-bottom: 24px;border: 0;overflow: hidden'>
          <template slot="header">
            <div class="collapse-panel-header-info">
              <span class="file-name">{{ item.fileName }}</span>
              <a-tooltip v-if="item.vulnerabilitiesCount > 0">
                <template slot="title">漏洞数量</template>
                <a-avatar :size="24" :src="'images/folib/bug.svg'" />
                <span class="mb-0 text-dark bug-count">{{ item.vulnerabilitiesCount }}</span>
              </a-tooltip>
              <a-tooltip v-else>
                <template slot="title">健康</template>
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
                  该依赖含有 <strong>{{ item.evidence.length }}</strong> 个风险凭证，并在扫描检测中发现
                  <strong>{{ item.vulnerabilitiesCount }}</strong>个漏洞
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
                  版本号: <strong>{{ item.version }}</strong>
                </p>

              </a-col>
            </a-row>
            <hr class="gradient-line">

            <a-row :gutter="[24]" type="flex" class="order-products" align="middle">
              <a-col :span="24" :md="12">
                <div class="d-flex">
                  <a-avatar class="mr-15" :src="'images/folib/' + getImage(item.ecosystem) + '.svg'" shape="square"
                    :size="80" />
                  <div>
                    <h6 class="mb-0 mt-10 font-semibold">{{ item.name }}</h6>
                    <p class="mb-15">
                      License: <strong>{{ item.license }}</strong>
                    </p>
                    <a-tag class="ant-tag-success font-semibold">{{ item.ecosystem }}</a-tag>
                  </div>
                </div>
              </a-col>
              <a-col :span="24" :md="12" class="ml-auto text-right">
                <p>{{ item.description }}</p>
              </a-col>
            </a-row>

            <hr class="gradient-line">

            <a-row :gutter="[24]" type="flex">
              <a-col :span="24" :md="24" :lg="24">
                <a-table :columns="vulnerColumns" :data-source="item.vulnerabilities" :pagination="false">

                  <a-row slot="expandedRowRender" :gutter="[24, 24]" slot-scope="record">
                    <a-col :span="24">
                      <a-card :bordered="false" class="card-billing-info">
                        <div class="col-info">
                          <a-descriptions :title="record.references.length + '个参考信息'" :column="1">
                            <a-descriptions-item label="说明">
                              以下信息均来自于开源社区
                            </a-descriptions-item>
                            <a-descriptions-item label="相关信息链接">
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
                      <a-avatar v-if="['CRITICAL', 'MEDIUM', 'HIGH', 'LOW'].indexOf(highestSeverityText) != -1"
                        :size="24" :src="'images/folib/' + highestSeverityText.toLowerCase() + '.svg'" />
                      <a-avatar v-else shape="circle" :size="24">{{ highestSeverityText.slice(0, 1) }}</a-avatar>
                      <div class="avatar-info">
                        <p class="mb-0 text-dark">{{
                            highestSeverityText === 'CRITICAL' ? '严重' : highestSeverityText === 'MEDIUM' ? '中危' :
                              highestSeverityText === 'HIGH' ? '高危' : highestSeverityText === 'LOW' ? '低危' :
                                highestSeverityText
                        }}
                        </p>
                      </div>
                    </div>
                  </template>
                  <template slot="v2_exploitabilityScore" slot-scope="text, record">{{ record.cvssV2.score }}</template>
                  <template slot="v3_exploitabilityScore" slot-scope="text, record">{{ record.cvssV3.baseScore
                  }}</template>
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

    <a-drawer placement="right" width="20%" :title="vulnerabilityDrawerTitle" :visible="vulnerabilityDrawerVisible"
      @close="vulnerabilityDrawerClose()">
      <!-- <a-card class="header-solid"> -->
      <a-list item-layout="vertical" size="large" :data-source="vulnerabilityDrawerData"
        :pagination="vulnerabilityDrawerData.length === 0 ? false : { pageSize: 10, total: vulnerabilityDrawerData.length, showLessItems: true }">
        <a-list-item slot="renderItem" :key="index" slot-scope="item, index">
          {{ item }}
        </a-list-item>
      </a-list>
      <!-- </a-card> -->
    </a-drawer>

    <a-modal v-model="showOperationFormModal" :footer="null" :forceRender="true" :centered="true"
      :title="operationTitle" on-ok="showCopyFormModal = false">
      <a-form :form="operationForm" ref="operationForm" layout="vertical" @submit.prevent="handleOperationSubmit">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-item class="tags-field mb-10" label="目标仓库" :colon="false" ref="targetRepositories"
              prop="targetRepositories">
              <gb-ant-select-two-cascader allowClear placeholder="请选择目标仓库" v-decorator="[
                'targetRepositories',
                {
                  initialValue: [],
                  rules: [{ required: true, message: '请选择目标仓库', type: 'array' }]
                }
              ]" :selectOptionsConfig="{ key: 'key', value: 'key', text: 'name', children: 'children' }"
                dropdownClassName="customer-multiple-cascader" :treeData="repositories" />
            </a-form-item>
            <!-- <a-form-item class="tags-field mb-10" :colon="false" :label="customTitle" valuePropName="checked">
              <a-switch v-decorator="['custom',
                {
                  valuePropName: 'checked',
                  rules: [
                    { required: false, message: '' },
                  ],
                },
              ]" style="width:10%;" @change="customChange">
              </a-switch>
            </a-form-item> -->
            <a-form-item class="tags-field mb-10" v-if="!custom" label="目标目录" prop="path" :colon="false">
              <a-input v-decorator="['path',
                {
                  rules: [
                    { required: true, message: '请输入目标目录' },
                  ],
                },
              ]" :disabled="true" placeholder="请输入目标目录">
              </a-input>
            </a-form-item>
            <a-form-item class="tags-field mb-10" v-if="custom" label="目标目录" prop="path" :colon="false">
              <a-input v-decorator="['path',
                {
                  rules: [
                    { required: true, message: '请输入目标目录' },
                  ],
                },
              ]" :disabled="false" placeholder="请输入目标目录">
              </a-input>
            </a-form-item>
          </a-col>
          <a-col :span="24" class="text-center">
            <a-button key="submit" class="px-30" size="small" type="primary" htmlType="submit">提交</a-button>
            <a-button key="back" @click="operationFormModalClose()" class="px-30 ml-10" size="small">取消</a-button>
          </a-col>
        </a-row>
      </a-form>
    </a-modal>

    <!--    rpm 上传表单 start-->
    <a-modal v-model="showRpmUploadFormModal" :footer="null" :forceRender="true" :centered="true" title="上传"
             on-ok="showRpmUploadFormModal = false">
      <a-form :form="rpmUploadForm" ref="rpmUploadForm" layout="horizontal" @submit.prevent="handleRpmUploadSubmit">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-item class="tags-field mb-10" label="目标仓库" prop="repostoryId" :colon="false">
              <a-input v-decorator="['repostoryId',
                {
                  rules: [
                    { required: true, message: '请输入目标仓库' },
                  ],
                },
              ]" :disabled="true" placeholder="请输入目标仓库">
              </a-input>
            </a-form-item>
            <a-form-item label="选择文件">
              <a-upload v-decorator="[
                'files',
                {
                  rules: [
                    { required: true, message: '请选择文件' },
                  ],
                  valuePropName: 'fileList',
                  getValueFromEvent: normFile,
                },
              ]" name="files" :multiple="true" :beforeUpload="beforeUpload" list-type="text">
                <a-button>
                  <a-icon type="upload" />选择文件
                </a-button>
              </a-upload>
            </a-form-item>
          </a-col>
          <a-col :span="24" class="text-center">
            <a-button key="submit" class="px-30" size="small" type="primary" htmlType="submit">上传</a-button>
            <a-button key="back" @click="uploadRpmFormModalClose()" class="px-30 ml-10" size="small">取消</a-button>
          </a-col>
        </a-row>
      </a-form>
    </a-modal>
    <!--   rpm 上传表单 end -->

    <a-modal v-model="showUploadFormModal" :footer="null" :forceRender="true" :centered="true" title="上传"
      on-ok="showUploadFormModal = false">
      <a-form :form="uploadForm" ref="uploadForm" layout="horizontal" @submit.prevent="handleUploadSubmit">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-item class="tags-field mb-10" label="目标仓库" prop="repostoryId" :colon="false">
              <a-input v-decorator="['repostoryId',
                {
                  rules: [
                    { required: true, message: '请输入目标仓库' },
                  ],
                },
              ]" :disabled="true" placeholder="请输入目标仓库">
              </a-input>
            </a-form-item>
            <a-form-item label="选择文件">
              <a-upload v-decorator="[
                'files',
                {
                  rules: [
                    { required: true, message: '请选择文件' },
                  ],
                  valuePropName: 'fileList',
                  getValueFromEvent: normFile,
                },
              ]" name="files" :multiple="true" :beforeUpload="beforeUpload" list-type="text">
                <a-button>
                  <a-icon type="upload" />选择文件
                </a-button>
              </a-upload>
            </a-form-item>
            <a-form-item class="tags-field mb-10" label="目标目录" prop="targetPath" :colon="false">
              <a-input v-decorator="['targetPath',
                {
                  rules: [
                    { required: false, message: '请输入目标目录' },
                    // { pattern: /^[a-zA-Z_]([a-zA-Z0-9_\-.\\/]+)?$/, message: '目标目录为大小写字母、数字、下划线开头，包含字母、数字、下划线、中划线、点、斜杠'}
                  ],
                },
              ]" placeholder="请输入目标目录">
              </a-input>
            </a-form-item>
          </a-col>
          <a-col :span="24" class="text-center">
            <a-button key="submit" class="px-30" size="small" type="primary" htmlType="submit">上传</a-button>
            <a-button key="back" @click="uploadFormModalClose()" class="px-30 ml-10" size="small">取消</a-button>
          </a-col>
        </a-row>
      </a-form>
    </a-modal>

    <a-modal v-model="showDeleteModal" :footer="null" :forceRender="true" :centered="true" title="确定删除吗？"
      on-ok="showDeleteModal = false" class="delete-modal">
      <a-row :gutter="[24]">
        <a-col :span="24">
          <p>不要冲动，再好好想想</p>
        </a-col>
        <a-col :span="24" class="text-right">
          <a-button @click="deletePackageHandle" class="mr-10" type="danger">删除</a-button>
          <a-button @click="showDeleteModal = false" type="default">取消</a-button>
        </a-col>
      </a-row>
    </a-modal>
  </div>
</template>

<script>
import storage from 'store'
import CardPackageTree from '@/components/Cards/CardPackageTree'
import CardProfileInformation from '../../components/Cards/CardProfileInformation'
import Vulnerability from '@/components/Vulnerabilities/Vulnerability'
import {
  getLayoutType,
  getFileType,
  fileSizeConver,
  formateDate
} from '@/utils/layoutUtil'
import { browse, getArtifact, viewArtifactFile, fql, scannerRules, insertOrUpdateRules, getDockerArtifact, deleteArtifact, getSeverity, repositoryVulnerabilityStatistics, getStoragesAndRepositories, } from '@/api/folib'
import { artifactCopy, artifactMove, artifactUpload,rpmArtifactUpload } from '@/api/artifact'
import { PrismEditor } from 'vue-prism-editor'
import 'vue-prism-editor/dist/prismeditor.min.css' // import the styles somewhere
// import highlighting library (you can use any library you want just return html string)
import { highlight, languages } from 'prismjs/components/prism-core'
import 'prismjs/components/prism-clike'
import 'prismjs/components/prism-javascript'
import 'prismjs/themes/prism-tomorrow.css'
import SearchBox from '@/components/Tools/SearchBox'
import zhCN from 'ant-design-vue/es/locale/zh_CN'
import { JSONLoader } from '../../plugins/three/threejs'
export default {
  inject: ["reload"],
  components: {
    CardPackageTree,
    CardProfileInformation,
    PrismEditor,
    SearchBox,
    Vulnerability,
  },
  data() {
    return {
      deleteVisible: false,
      ivyCode: null,
      dockerCode: { ubuntu: null, centos: null, windows: null, macos: null },
      scan: {
        id: '',
        repository: "",
        storage: "",
        onScan: false,
        scanRule: null,
        layout: null
      },
      usedVisible: false,
      isNotSearch: false,
      viewCodeVisible: false,
      //包列表树形组件数据
      expandedKeys: [],
      selectedKeys: [],
      treeData: [],
      repositoryType: null,
      folibRepository: {},
      baseUrl: '',
      currentChecked: 'detial',
      currentTreeNode: {},
      currentFileDetial: null,
      currentManifest: {},
      codeParam: {
        type: '',
        code: null
      },
      viewCodes: null,
      mouseEnter: false,
      snippets: [],
      artifactQuery: {
        artifactName: null,
        storageId: null,
        repositoryId: null,
        limit: 5,
        page: 1,
        total: 0,
        sortField: null,
        sortOrder: null,
        beginDate: null,
        endDate: null,
      },
      searchData: [],
      searchDataCurrentSelect: {},
      searchViewCodeVisible: false,
      searchViewCodes: null,
      // Table columns
      columns: [
        {
          title: '制品路径',
          dataIndex: 'path',
          scopedSlots: { customRender: 'path' },
          width: 550,
        },
        {
          title: '创建时间',
          dataIndex: 'created',
          sorter: true,
          sortDirections: ['descend', 'ascend'],
          scopedSlots: { customRender: 'created' },
          width: 200,
        },
        {
          title: '最近使用时间',
          dataIndex: 'lastUsed',
          sorter: true,
          scopedSlots: { customRender: 'lastUsed' },
          width: 200,
        },
        {
          title: '下载次数',
          dataIndex: 'downloadCount',
          sorter: true,
          scopedSlots: { customRender: 'created' },
          width: 200,
        },
        {
          title: '制品大小',
          dataIndex: 'sizeInBytes',
          sorter: true,
          scopedSlots: { customRender: 'sizeInBytes' },
          width: 200,
        },
      ],
      severity: { show: false },
      detialVisible: false,
      currentReport: [],
      vulnerColumns: [
        {
          title: 'CVE编号',
          dataIndex: 'name',
          scopedSlots: { customRender: 'name' },
        },
        {
          title: '漏洞等级',
          dataIndex: 'highestSeverityText',
          scopedSlots: { customRender: 'highestSeverityText' },
        },
        {
          title: 'CvssV2评分',
          dataIndex: 'cvssV2',
          scopedSlots: { customRender: 'v2_exploitabilityScore' },
        },
        {
          title: 'CvssV3评分',
          dataIndex: 'cvssV3',
          scopedSlots: { customRender: 'v3_exploitabilityScore' },
        },
        {
          title: '引入版本',
          scopedSlots: { customRender: 'versionStartIncluding' }
        },
        {
          title: '建议修复版本',
          scopedSlots: { customRender: 'versionEndExcluding' }
        }
      ],
      vulnerabilityColumns: [
        {
          title: '漏洞编号',
          dataIndex: 'uuid',
          scopedSlots: { customRender: 'uuid' },
        },
        {
          title: '引入时间',
          dataIndex: 'created',
          scopedSlots: { customRender: 'created' },
          align: "center",
        },
        {
          title: 'CvssV2评分',
          dataIndex: 'cvssV2Score',
          scopedSlots: { customRender: 'cvssV2Score' },
          align: "center",
        },
        {
          title: 'CvssV2漏洞等级',
          dataIndex: 'cvssV2Severity',
          scopedSlots: { customRender: 'cvssV2Severity' },
          align: "center",
        },
        {
          title: 'CvssV3评分',
          dataIndex: 'cvssV3Score',
          scopedSlots: { customRender: 'cvssV3Score' },
          align: "center",
        },
        {
          title: 'CvssV3漏洞等级',
          dataIndex: 'cvssV3Severity',
          scopedSlots: { customRender: 'cvssV3Severity' },
          align: "center",
        },
        {
          title: '最高漏洞等级',
          dataIndex: 'highestSeverityText',
          scopedSlots: { customRender: 'highestSeverityText' },
          align: "center",
        },
        {
          title: '建议修复版本',
          dataIndex: 'versionEndExcluding',
          scopedSlots: { customRender: 'versionEndExcluding' },
        },
        {
          title: '操作',
          dataIndex: 'operation',
          scopedSlots: { customRender: 'operation' },
        },
      ],
      vulnerabilityStatistics: {
        artifactCount: 0,
        downloadCount: 0,
        dependencyCount: 0,
        vulnerabilityCount: 0,
        whiteCount: 0,
        blackCount: 0,
      },
      tabActiveKey: 1,
      vulnerabilityDrawerVisible: false,
      vulnerabilityDrawerTitle: '',
      vulnerabilityDrawerData: [],
      artifactVisible: false,
      locale: zhCN,
      showOperationFormModal: false,
      showUploadFormModal: false,
      showRpmUploadFormModal: false,
      operationTitle: '',
      customTitle: '',
      operationForm: this.$form.createForm(this, { name: 'operation_form' }),
      uploadForm: this.$form.createForm(this, { name: 'upload_form' }),
      rpmUploadForm: this.$form.createForm(this, { name: 'rpmUpload_form' }),
      repositories: [],
      storages: [],
      custom: false,
      showDeleteModal: false,
    }
  },
  created() {
    this.createData()
    this.getBrowse()
    this.scannerRules()
    this.repositoryVulnerabilityStatistics()
  },
  methods: {
    scannerRules() {
      scannerRules(this.folibRepository.storageId + "-" + this.folibRepository.id).then(res => {
        if (res.rel) {
          this.scan = res.data
        }
      })
    },
    scannerChange() {
      this.scan.id = this.folibRepository.storageId + "-" + this.folibRepository.id
      this.scan.repository = this.folibRepository.id
      this.scan.storage = this.folibRepository.storageId
      this.scan.layout = this.folibRepository.layout
      insertOrUpdateRules(this.scan).then(res => {
        setTimeout(() => {
          this.$notification.success({
            message: this.scan.onScan ? "开启扫描" : "关闭扫描"
          })
        }, 100)
      })
    },
    handleTableChange(pagination, filters, sorter) {
      this.artifactQuery.sortField = null
      this.artifactQuery.sortOrder = null
      if (pagination) {
        this.artifactQuery.page = pagination.current
      }
      if (sorter) {
        this.artifactQuery.sortField = sorter.field
        if (sorter.order) {
          this.artifactQuery.sortOrder = 'asc'
          if (sorter.order.indexOf("desc") !== -1) {
            this.artifactQuery.sortOrder = 'desc'
          }
        }
      }
      this.search(this.artifactQuery.artifactName)
    },
    onPageSizeChange() {
      this.search(this.artifactQuery.artifactName, 1)
    },
    searchBoxMouseStatus(bool) {
      this.mouseEnter = bool
    },
    search(value, page) {
      if (page) {
        this.artifactQuery.page = page
      }
      if (value) {
        this.artifactQuery.artifactName = value
      }
      this.tabActiveKey = 1
      this.artifactQuery.storageId = this.folibRepository.storageId
      this.artifactQuery.repositoryId = this.folibRepository.id
      let params = {
        artifactName: this.artifactQuery.artifactName,
        storageId: this.artifactQuery.storageId,
        repositoryId: this.artifactQuery.repositoryId,
        limit: this.artifactQuery.limit,
        page: this.artifactQuery.page,
        sortField: this.artifactQuery.sortField,
        sortOrder: this.artifactQuery.sortOrder,
        beginDate: this.artifactQuery.beginDate,
        endDate: this.artifactQuery.endDate,
        regex: false,
      }
      if (this.folibRepository.layout === 'Docker') {
        params.regex = true
        params.artifactName = ".*" + this.artifactQuery.artifactName + "((.(?!blobs/sha256|manifest/sha256))*.)";
      }
      fql(params).then(res => {
        this.searchData = res.artifact
        this.artifactQuery.total = res.total
      })
      this.isNotSearch = true
    },
    searchDataHandle(item) {
      this.searchDataCurrentSelect = item
      if (this.searchDataCurrentSelect && this.searchDataCurrentSelect.snippets) {
        this.changeCodeTye(this.searchDataCurrentSelect.snippets[0])
      }
      var id = "storages/" + this.searchDataCurrentSelect.storageId + "/" + this.searchDataCurrentSelect.repositoryId + "/" + this.searchDataCurrentSelect.path
      this.handlerSeverity(id)
      this.artifactVisible = true
    },
    closeSearchviewCodeDialog() {
      this.searchViewCodeVisible = false
      this.searchViewCodes = null
    },
    changeCodeTye(item) {
      if (item) {
        this.codeParam = { type: item.name === 'Maven 2' ? 'maven' : item.name.toLowerCase(), code: item.code }
      }
    },
    copy(url) {
      var input = document.createElement('input') // 创建input对象
      input.value = url // 设置复制内容
      document.body.appendChild(input) // 添加临时实例
      input.select() // 选择实例内容
      document.execCommand('Copy') // 执行复制
      document.body.removeChild(input) // 删除临时实例
      // console.log(url)
      setTimeout(() => {
        this.$notification.success({
          message: '复制成功'
        })
      }, 100)
    },
    createData() {
      //上个页面通过缓存传参，目的防止页面刷新，路由数据消失
      const params = storage.get('libView_repository')
      this.folibRepository = params.item
      this.baseUrl = params.baseUrl
      this.repositoryType = this.getLayoutTypeHandle()
      this.isNotSearch = false
    },
    getLayoutTypeHandle() {
      return getLayoutType(this.folibRepository)
    },
    getBrowse() {

      browse(this.folibRepository.storageId, this.folibRepository.id, '').then(
        res => {
          const d = res.directories
          d.forEach((item, index, d) => {
            item.type = 'dir'
          })
          const f = res.files
          f.forEach((item, index) => {
            item.isLeaf = true
            item.type = 'file'
          })
          this.treeData = d.concat(f)
        }
      )
    },
    onLoadData(treeNode) {
      this.currentFileDetial = null
      if (this.folibRepository.layout === 'Docker') {

        return new Promise(resolve => {
          if (treeNode.dataRef.children) {
            resolve()
            return
          }
          getDockerArtifact(
            this.folibRepository.storageId,
            this.folibRepository.id,
            treeNode.dataRef.artifactPath
          ).then(res => {
            if (res.directories.length > 0) {
              const d = res.directories
              d.forEach((item, index, d) => {
                item.type = 'dir'
              })
              treeNode.dataRef.children = d
            } else if (res.files.length > 0) {
              const a = res.files
              a.forEach((item, index, a) => {
                item.isLeaf = true
                item.type = 'file'
              })
              treeNode.dataRef.children = a
            }

            this.treeData = [...this.treeData]
            resolve()
          })
        })
      } else {
        return new Promise(resolve => {
          if (treeNode.dataRef.children) {
            resolve()
            return
          }
          browse(
            this.folibRepository.storageId,
            this.folibRepository.id,
            treeNode.dataRef.artifactPath
          ).then(res => {
            if (res.directories.length > 0) {
              const d = res.directories
              d.forEach((item, index, d) => {
                item.type = 'dir'
              })
              treeNode.dataRef.children = d
            } else if (res.files.length > 0) {
              const a = res.files
              a.forEach((item, index, a) => {
                item.isLeaf = true
                item.type = 'file'
              })
              treeNode.dataRef.children = a
            }

            this.treeData = [...this.treeData]
            resolve()
          })
        })

      }

    },
    treeSelect(key, e) {
      this.currentTreeNode = e.node.dataRef

      if (this.currentTreeNode.type === 'file') {
        getArtifact(
          this.repositoryType,
          this.currentTreeNode.storageId,
          this.currentTreeNode.repositoryId,
          this.currentTreeNode.artifactPath
        ).then(res => {
          this.currentFileDetial = res
          if (this.currentFileDetial.snippets) {
            this.changeCodeTye(this.currentFileDetial.snippets[0])
          }
          this.currentManifest = res.manifestConfig
        })
        this.handlerSeverity()
      } else if (this.currentTreeNode.type === 'dir') {
        this.currentFileDetial = null
        this.severity = { show: false }
      }
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
    formateDate(time) {
      if (time) {
        var date = new Date(time)
        var Y = date.getFullYear() + '-'
        var M = (date.getMonth() + 1 < 10 ? "0" + (date.getMonth() + 1) : (date.getMonth() + 1)) + "-"
        var D = (date.getDate() < 10 ? "0" + (date.getDate()) : date.getDate()) + " "
        var h = (date.getHours() < 10 ? "0" + (date.getHours()) : date.getHours()) + ':'
        var m = (date.getMinutes() < 10 ? "0" + (date.getMinutes()) : date.getMinutes()) + ':'
        var s = (date.getSeconds() < 10 ? "0" + (date.getSeconds()) : date.getSeconds()) + ''
        return Y + M + D + h + m + s
      }
    },
    highlighterHandle(code) {
      return highlight(code, languages.js) //returns html
    },
    closeViewCodeDialog() {
      this.viewCodeVisible = false
      this.viewCodes = null
    },
    viewCodeHandle() {
      if (this.folibRepository.layout !== 'Docker') {
        if (this.currentFileDetial && !this.currentFileDetial.listTree) {
          viewArtifactFile(this.currentTreeNode.url).then(res => {
            this.viewCodes = res
          })
        }
      } else {
        // this.viewCodes=this.currentManifest.config
      }

      this.viewCodeVisible = true
    },
    searchViewCodeHandle() {
      if (this.searchDataCurrentSelect && !this.searchDataCurrentSelect.treeNode) {
        viewArtifactFile(this.searchDataCurrentSelect.url).then(res => {
          this.searchViewCodes = res
        })
      }
      this.searchViewCodeVisible = true
    },
    getCodeImg(item) {
      return item.name === 'Maven 2' ? 'maven_black' : item.name.toLowerCase()
    },
    closeUsedVisibleDialog() {
      this.usedVisible = false
    },
    UsedHelperVisible() {
      if (this.repositoryType === 'ivy') {
        this.ivyCode = "<ivysettings>\n" +
          "   <settings defaultResolver=\"" + this.folibRepository.id + "\" defaultConflictManager=\"all\" />\n" +
          "   <resolvers>\n" +
          "        <ibiblio name=\"releases\" root=\"" + this.baseUrl + 'storages/' + this.folibRepository.storageId + '/' + this.folibRepository.id + "\" m2compatible=\"true\" usepoms=\"true\"/>\n" +
          "   </resolvers>\n" +
          "</ivysettings>"
      } else if (this.repositoryType === 'docker') {
        this.dockerCode.ubuntu = "sudo mkdir -p /etc/docker\n" +
          "sudo tee /etc/docker/daemon.json <<-'EOF'\n" +
          "{\n" +
          "\"insecure-registries\": [\"" + this.baseUrl.replace("http://", "").replace("/", "") + "\"]\n" +
          "}\n" +
          "EOF\n" +
          "sudo systemctl daemon-reload\n" +
          "sudo systemctl restart docker"
        this.dockerCode.centos = "sudo mkdir -p /etc/docker\n" +
          "sudo tee /etc/docker/daemon.json <<-'EOF'\n" +
          "{\n" +
          "\"insecure-registries\": [\"" + this.baseUrl.replace("http://", "").replace("/", "") + "\"]\n" +
          "}\n" +
          "EOF\n" +
          "sudo systemctl daemon-reload\n" +
          "sudo systemctl restart docker"
        this.dockerCode.macos = this.baseUrl
        this.dockerCode.windows = "{\n" +
          "  \"insecure-registries\": [\"" + this.baseUrl.replace("http://", "").replace("/", "") + "\"]\n" +
          "}"
      }
      this.usedVisible = true
    },
    deletePackageHandle() {
      deleteArtifact(this.currentTreeNode.storageId, this.currentTreeNode.repositoryId, this.currentTreeNode.artifactPath).then(res => {
        setTimeout(() => {
          this.$notification.success({
            message: '删除成功'
          })
          this.reload()
        }, 100)
      }).catch((err) => {
        this.$notification["error"]({
          message: "删除失败",
          description: ""
        })
      }).finally(() => {
      })
    },
    handlerSeverity(id) {
      this.severity = { show: false }
      if (!id) {
        id = "storages/" + this.currentTreeNode.storageId + "/" + this.currentTreeNode.repositoryId + "/" + this.currentTreeNode.artifactPath
      }
      var flag = id.endsWith('.sha') || id.endsWith('.sha1') || id.endsWith('.sha256') || id.endsWith('.sha512') || id.endsWith('.md5')
      if (flag) {
        return
      }
      getSeverity(id).then(res => {
        if (res.rel) {
          this.severity = res.data
          if (this.severity.report) {
            this.currentReport = JSON.parse(this.severity.report)
          }
        }
      })
    },
    closeDialog() {
      this.detialVisible = false
    },
    getImage(ecosystem) {
      return ecosystem ? ecosystem : this.getLayoutTypeHandle()
    },
    repositoryVulnerabilityStatistics() {
      repositoryVulnerabilityStatistics({ storageId: this.folibRepository.storageId, repositoryId: this.folibRepository.id }).then(res => {
        this.vulnerabilityStatistics = res
      })
    },
    tabChange(activeKey) {
      this.tabActiveKey = activeKey
      if (activeKey == 2) {
        if (this.$refs.vulnerability) {
          this.$refs.vulnerability.getVulnerabilityPage()
        }
      }
    },
    goBack() {
      this.$router.push({ name: 'storages' })
    },
    vulnerabilityDrawerShow(type) {
      this.vulnerabilityDrawerVisible = true
      if (type === 1) {
        this.vulnerabilityDrawerTitle = '白名单'
        this.vulnerabilityDrawerData = this.folibRepository.vulnerabilityWhites
      }
      if (type === 2) {
        this.vulnerabilityDrawerTitle = '黑名单'
        this.vulnerabilityDrawerData = this.folibRepository.vulnerabilityBlacks
      }
    },
    vulnerabilityDrawerClose() {
      this.vulnerabilityDrawerVisible = false
    },
    successMsg(message) {
      if (!message) {
        message = "操作成功"
      }
      this.$notification["success"]({
        message: message,
        description: ""
      })
    },
    dateChange(value, dateString) {
      if (dateString) {
        this.artifactQuery.beginDate = dateString[0]
        this.artifactQuery.endDate = dateString[1]
        if (this.artifactQuery.beginDate === '' && this.artifactQuery.endDate === '') {
          this.dateConfirm()
        }
      }
    },
    dateConfirm() {
      this.search(this.artifactQuery.artifactName, 1)
    },
    handleRpmUpload(){
      this.rpmUploadForm.resetFields()
      this.$nextTick(() => {
        if (this.$refs.rpmUploadForm) {
          this.rpmUploadForm.setFieldsValue({
            repostoryId: this.folibRepository.id,
          })
        }
      })
      this.showRpmUploadFormModal = true
    },
    handleUpload() {
      this.uploadForm.resetFields()
      this.$nextTick(() => {
        if (this.$refs.uploadForm) {
          this.uploadForm.setFieldsValue({
            repostoryId: this.folibRepository.id,
          })
        }
      })
      this.showUploadFormModal = true
    },
    uploadFormModalClose() {
      this.showUploadFormModal = false
    },
    uploadRpmFormModalClose() {
      this.showRpmUploadFormModal = false
    },
    handleMenuClick(active) {
      this.operationForm.resetFields()
      this.$nextTick(() => {
        if (this.$refs.operationForm) {
          this.operationForm.setFieldsValue({
            path: this.currentTreeNode.artifactPath,
          })
        }
      })
      if (active.key === '1' || active.key === '2') {
        //复制 或 移动
        this.showOperationFormModal = true
        this.getStoragesAndRepositories(this.folibRepository.type, this.folibRepository.layout, this.folibRepository.id, this.folibRepository.policy)
        this.operationTitle = active.key === '1' ? '复制 ' + this.currentTreeNode.artifactPath : '移动  ' + this.currentTreeNode.artifactPath
        this.customTitle = active.key === '1' ? '复制到自定义目录' : '移动到自定义目录'
      } else if (active.key === '3') {
        //删除
        this.showDeleteModal = true
      }
    },
    getStoragesAndRepositories(type, layout, excludeRepositoryId, policy) {
      getStoragesAndRepositories({ type: type, layout: layout, excludeRepositoryId: excludeRepositoryId, policy: policy }).then(res => {
        this.repositories = []
        res.forEach(item => {
          if (item.children && item.children.length > 0) {
            this.repositories.push(item)
          }
        })
      })
    },
    customChange(value) {
      this.custom = value
      if (!value) {
        this.$nextTick(() => {
          if (this.$refs.operationForm) {
            this.operationForm.setFieldsValue({
              path: this.currentTreeNode.artifactPath,
            })
          }
        })
      }
    },
    operationFormModalClose() {
      this.showOperationFormModal = false
    },
    handleOperationSubmit(e) {
      e.preventDefault()
      this.operationForm.validateFields((err, values) => {
        if (!err) {
          let targetRepositoyList = []
          values.targetRepositories.forEach(item => {
            let split = item.split(",")
            targetRepositoyList.push({ targetStorageId: split[0], targetRepositoryId: split[1] })
          })
          let data = {
            path: values.path,
            srcStorageId: this.folibRepository.storageId,
            srcRepositoryId: this.folibRepository.id,
            targetRepositoyList: targetRepositoyList
          }
          if (this.operationTitle.indexOf('复制') !== -1) {
            artifactCopy(data).then(res => {
              this.successMsg('复制中，请稍候查看')
              this.operationFormModalClose()
              this.reload()
            }).catch((err) => {
              this.$notification["error"]({
                message: err.response.data.error,
                description: ""
              })
            }).finally(() => {
            })
          } else if (this.operationTitle.indexOf('移动') !== -1) {
            artifactMove(data).then(res => {
              this.successMsg("移动中，请稍候查看")
              this.operationFormModalClose()
              this.reload()
            }).catch((err) => {
              this.$notification["error"]({
                message: err.response.data.error,
                description: ""
              })
            }).finally(() => {
            })
          }
        }
      });
    },
    beforeUpload(file, fileList) {
      return false
    },
    normFile(e) {
      if (Array.isArray(e)) {
        return e;
      }
      return e && e.fileList;
    },
    handleRpmUploadSubmit(e){
      e.preventDefault()
      this.rpmUploadForm.validateFields((err, values) => {
        if (!err) {
          const rpmFormData = new FormData();
          values.files.forEach(item => {
            rpmFormData.append('files', item.originFileObj)
          })
          rpmArtifactUpload(
              this.folibRepository.storageId,
              this.folibRepository.id,
              rpmFormData
          ).then(res => {
            this.successMsg("上传成功")
            this.uploadRpmFormModalClose()
            this.reload()
          }).catch((err) => {
            this.$notification["error"]({
              message: err.response.data.error,
              description: ""
            })
          }).finally(() => {
          })
        }
      })
      debugger
    },

    handleUploadSubmit(e) {
      e.preventDefault()
      this.uploadForm.validateFields((err, values) => {
        if (!err) {
          let filePathMap = {};
          let fileList = [];
          values.files.forEach(item => {
            filePathMap[item.name] = values.targetPath ? values.targetPath + '/' + item.name : item.name
            fileList.push(item.originFileObj)
          })
          values.filePathMap = filePathMap
          const formData = new FormData();
          formData.append("storageId", this.folibRepository.storageId);
          formData.append("repostoryId", this.folibRepository.id);
          formData.append("filePathMap", JSON.stringify(filePathMap));
          fileList.forEach((file) => {
            formData.append('files', file)
          })
          artifactUpload(formData).then(res => {
            this.successMsg("上传成功")
            this.uploadFormModalClose()
            this.reload()
          }).catch((err) => {
            this.$notification["error"]({
              message: err.response.data.error,
              description: ""
            })
          }).finally(() => {
          })
        }
      })
    },
  }
}
</script>

<style lang="scss" scoped>
$md: 768px;

.lib-view::v-deep {

  .profile-nav-bg {
    display: flex;
    justify-content: center;
    align-items: center;
    color: #fafafa;
    position: relative;
    overflow: hidden;
    width: 100%;
  }

  .statistics-bg {
    height: 75px !important;
  }

  .my-editor {
    background: #fafafa;
    color: #595959;

    font-family: Fira code, Fira Mono, Consolas, Menlo, Courier, monospace;
    font-size: 12px;
    line-height: 1.5;
    padding: 5px;
  }

  // optional
  .prism-editor__textarea:focus {
    outline: none;
  }

  // not required:
  .height-80 {
    height: 80px;
  }

  .mouse-enter {
    transform: scale(1.3);
    transition: all .3s;
  }

  .nested {
    position: absolute;
    left: 0;
    right: 0;
    top: 0;
    bottom: 0;
  }

  //search列表
  .table-avatar-info {
    display: flex;
    align-items: center;
  }

  .table-avatar-info .ant-avatar {
    margin-right: 8px;
  }

  .table-severity-info {
    display: inline-block;
    height: 30px;
    margin-left: 30px;
  }

  .severity-info {
    display: inline-block;
    width: 40px;
    margin-right: 20px;
  }

  // Using vuejs "Deep Selectors"
  .table-avatar-info::v-deep .ant-avatar-string {
    font-size: 12px;
  }

  .btn-status::v-deep .anticon {
    line-height: 0;
  }

  .collapse-panel-header-info {
    display: inline-block;
  }

  .collapse-panel-header-info .file-name,
  .bug-count {
    margin-right: 10px;
  }

  .collapse-panel-header-info .bug-count {
    vertical-align: middle;
    margin-left: 2.5px;
  }

  .repository-affix {
    margin-top: 50px;
  }

  .card-profile-head {
    margin: -53px 0px 24px;
  }

  .widget-2 .icon svg path {
    fill: #FFFFFF;
  }

  .vulnerability-count {
    cursor: pointer;
  }

  .delete-modal>.ant-modal {
    min-width: 200px;
  }

  .search-column-path {
    white-space: pre-line;
    width: calc(100% - 24px);
  }
}

.delete-modal>.ant-modal {
  min-width: 200px;
}

.d-popconfirm {
  height: 34px;
  font-size: 12px;
  font-weight: 600;
  margin-right: 20px;
}

.d-popconfirm>svg+span {
  vertical-align: middle;
  display: inline-block;
  transition: margin-left 0.3s cubic-bezier(0.645, 0.045, 0.355, 1);
  pointer-events: none;
}

.d-popconfirm svg {
  vertical-align: middle;
  margin-right: 5px;
}
</style>