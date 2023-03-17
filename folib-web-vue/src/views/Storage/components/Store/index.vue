<template>
  <div>
    <a-affix :offset-top="50" class="repository-affix">
      <a-row>
        <a-col :span="24" :md="24" class="mb-24">
          <!-- User Profile Card -->
          <a-card
            :bordered="false"
            class="card-profile-head"
            :bodyStyle="{ padding: 0 }"
            :targetOffset="0"
            :affix="false"
          >
            <template #title>
              <a-row type="flex" align="middle">
                <a-col :span="24" :md="12" class="col-info">
                  <a>
                    <a-icon
                      type="backward"
                      :style="{
                        fontSize: '32px',
                        marginRight: '20px',
                        opacity: '0.8',
                        color: '#BFBFBFFF',
                      }"
                      @click="goBack()"
                    />
                  </a>
                  <a>
                    <a-avatar
                      @click="createData"
                      :size="54"
                      shape="square"
                      style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
                      :src="'images/folib/' + getLayoutTypeHandle() + '.svg'"
                    />
                  </a>
                  <div class="avatar-info">
                    <a>
                      <h4 class="font-semibold m-0" @click="createData">
                        {{ folibRepository.id }}
                      </h4>
                    </a>
                    <p>
                      {{ baseUrl }}api/browse/{{ folibRepository.storageId }}/{{
                        folibRepository.id
                      }}
                      <a>
                        <a-icon
                          type="copy"
                          @click="
                            copy(
                              baseUrl +
                                'api/browse/' +
                                folibRepository.storageId +
                                '/' +
                                folibRepository.id
                            )
                          "
                        />
                      </a>
                    </p>
                  </div>
                </a-col>
                <a-col
                  :span="24"
                  :md="12"
                  style="
                    display: flex;
                    align-items: center;
                    justify-content: flex-end;
                  "
                >
                  <a v-if="folibRepository.layout === 'rpm'">
                    <small style="padding-right: 20px" @click="handleRpmUpload">
                      上传
                      <a-icon type="cloud-upload" />
                    </small>
                  </a>
                  <a
                    v-if="uploadEnabled"
                    ><small style="padding-right: 20px" @click="handleUpload">
                      上传
                      <a-icon type="cloud-upload" />
                    </small>
                  </a>
                  <a v-if="folibRepository.layout !== 'Raw'">
                    <small
                      style="padding-right: 20px"
                      @click="UsedHelperVisible"
                    >
                      使用帮助
                      <a-icon type="question-circle" theme="filled" />
                    </small>
                  </a>
                  <div v-if="folibRepository.type !== 'group'">
                    <span class="mr-15">{{
                      scan.onScan ? "扫描开启" : "扫描关闭"
                    }}</span>
                    <a-switch
                      default-checked
                      v-model="scan.onScan"
                      @change="scannerChange"
                    />
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
        <a-card
          :bordered="false"
          style="max-height: 1024px; min-height: 454px; overflow-y: auto"
          class="header-solid"
          :bodyStyle="{ paddingTop: 0, paddingBottom: 0 }"
        >
          <template #title>
            <h6 class="font-semibold m-0">包列表 <a class="ml-10" @click="reload()"><a-icon type="reload" /></a></h6>
          </template>
          <a-directory-tree
            :replaceFields="{
              key: 'artifactPath',
              title: 'name',
              children: 'children',
            }"
            :tree-data="treeData"
            :load-data="onLoadData"
            @select="treeSelect"
          >
          </a-directory-tree>
        </a-card>
      </a-col>

      <a-col :span="24" :md="14" class="mb-24">
        <a-card
          :bordered="false"
          class="header-solid h-full card-profile-information"
          :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }"
          :headStyle="{ paddingRight: 0 }"
        >
          <template #title>
            <a-row
              type="flex"
              align="middle"
              v-if="folibRepository.layout !== 'Docker'"
            >
              <a-col :span="16" class="font-semibold m-0">
                <a-row type="flex" align="middle">
                  <a-col :span="8" :xs="24" :xl="16">
                    <a-avatar
                      v-if="!currentTreeNode.isLeaf"
                      :size="24"
                      shape="square"
                      :src="'images/folib/package.svg'"
                    />
                    <a-avatar
                      v-if="currentTreeNode.isLeaf"
                      :size="24"
                      shape="square"
                      :src="
                        'images/folib/' +
                        getFileType(currentTreeNode.name) +
                        '.svg'
                      "
                    />
                    {{ currentTreeNode.name }}
                  </a-col>
                  <a-col :span="8" :xs="24" :xl="8">
                    <span
                      class="ml-auto"
                      v-if="scanReport.show"
                      @click="openDetial"
                    >
                      <a-space :size="1" class="avatar-chips">
                        <template v-if="scanReport.vulnerabilitesCount > 0">
                          <a-tooltip>
                            <template slot="title">严重</template>
                            <div class="">
                              <a-avatar
                                :size="24"
                                :src="'images/folib/critical.svg'"
                              />
                              <span class="mb-0 text-dark">{{
                                scanReport.critical
                              }}</span>
                            </div>
                          </a-tooltip>

                          <a-tooltip>
                            <template slot="title">高危</template>
                            <div class="">
                              <a-avatar
                                :size="24"
                                :src="'images/folib/high.svg'"
                              />
                              <span class="mb-0 text-dark">{{
                                scanReport.high
                              }}</span>
                            </div>
                          </a-tooltip>

                          <a-tooltip>
                            <template slot="title">中危</template>
                            <div class="">
                              <a-avatar
                                :size="24"
                                :src="'images/folib/medium.svg'"
                              />
                              <span class="mb-0 text-dark">{{
                                scanReport.medium
                              }}</span>
                            </div>
                          </a-tooltip>

                          <a-tooltip>
                            <template slot="title">低危</template>
                            <div class="">
                              <a-avatar
                                :size="24"
                                :src="'images/folib/low.svg'"
                              />
                              <span class="mb-0 text-dark">{{
                                scanReport.low
                              }}</span>
                            </div>
                          </a-tooltip>
                        </template>
                        <template v-else>
                          <a-tooltip>
                            <template slot="title">健康</template>
                            <a-avatar
                              :size="24"
                              :src="'images/folib/healthy.svg'"
                            />
                          </a-tooltip>
                        </template>
                      </a-space>
                    </span>
                  </a-col>
                </a-row>
              </a-col>
              <a-col :span="8" class="text-right">
                <a-dropdown
                  v-if="currentTreeNode.url"
                  class="mr-30"
                  placement="bottomCenter"
                >
                  <span style="font-size: 16px; cursor: pointer">
                    更多
                    <a-icon
                      type="more"
                      class="text-muted"
                      style="font-size: 16px"
                    />
                  </span>
                  <template #overlay>
                    <a-menu slot="overlay" @click="handleMenuClick">
                      <a-menu-item key="1" v-if="currentFileDetial">
                        <a-icon type="eye" />
                        {{
                          currentFileDetial.listTree
                            ? "包"
                            : viewCodes
                            ? "文件"
                            : folibRepository.layout === "Docker"
                            ? "详情"
                            : ""
                        }}预览
                      </a-menu-item>
                      <a-menu-item
                        key="2"
                        v-if="copyEnabled"
                      >
                        <a-icon type="copy" />复制
                      </a-menu-item>
                      <a-menu-item
                        key="3"
                        v-if="moveEnabled"
                      >
                        <a-icon type="swap" />移动
                      </a-menu-item>
                      <a-menu-item key="4"
                      v-if="deleteEnabled">
                        <a-popconfirm
                          title="确定要删除吗？"
                          placement="topLeft"
                          okType="danger"
                          ok-text="确定"
                          cancel-text="取消"
                          @confirm="deletePackageHandle"
                        >
                          <a-icon type="delete" />删除
                        </a-popconfirm>
                      </a-menu-item>
                      <a-menu-item
                        key="5"
                        v-if="folibRepository.type !== 'group' &&
                          currentFileDetial &&
                          currentFileDetial.artifact &&
                          currentFileDetial.artifact.artifactFileExists
                        "
                      >
                        <a-icon type="database" />元数据
                      </a-menu-item>
                    </a-menu>
                  </template>
                </a-dropdown>
              </a-col>
            </a-row>
            <a-row
              type="flex"
              align="middle"
              v-if="folibRepository.layout === 'Docker'"
            >
              <a-col :span="16" class="font-semibold m-0">
                <a-row type="flex" align="middle">
                  <a-col :span="8" :xs="24" :xl="16">
                    <a-avatar
                      :size="24"
                      shape="square"
                      :src="'images/folib/docker-s.svg'"
                    />
                    {{ currentTreeNode.name }}
                  </a-col>
                  <a-col :span="8" :xs="24" :xl="8">
                    <span
                      class="ml-auto"
                      v-if="scanReport.show"
                      @click="openDetial"
                    >
                      <a-space :size="1" class="avatar-chips">
                        <template v-if="scanReport.vulnerabilitesCount > 0">
                          <a-tooltip>
                            <template slot="title">严重</template>
                            <div class="">
                              <a-avatar
                                :size="24"
                                :src="'images/folib/critical.svg'"
                              />
                              <span class="mb-0 text-dark">{{
                                scanReport.critical
                              }}</span>
                            </div>
                          </a-tooltip>

                          <a-tooltip>
                            <template slot="title">高危</template>
                            <div class="">
                              <a-avatar
                                :size="24"
                                :src="'images/folib/high.svg'"
                              />
                              <span class="mb-0 text-dark">{{
                                scanReport.high
                              }}</span>
                            </div>
                          </a-tooltip>

                          <a-tooltip>
                            <template slot="title">中危</template>
                            <div class="">
                              <a-avatar
                                :size="24"
                                :src="'images/folib/medium.svg'"
                              />
                              <span class="mb-0 text-dark">{{
                                scanReport.medium
                              }}</span>
                            </div>
                          </a-tooltip>

                          <a-tooltip>
                            <template slot="title">低危</template>
                            <div class="">
                              <a-avatar
                                :size="24"
                                :src="'images/folib/low.svg'"
                              />
                              <span class="mb-0 text-dark">{{
                                scanReport.low
                              }}</span>
                            </div>
                          </a-tooltip>
                        </template>
                        <template v-else>
                          <a-tooltip>
                            <template slot="title">健康</template>
                            <a-avatar
                              :size="24"
                              :src="'images/folib/healthy.svg'"
                            />
                          </a-tooltip>
                        </template>
                      </a-space>
                    </span>
                  </a-col>
                </a-row>
              </a-col>
              <a-col :span="8" class="text-right">
                <a-dropdown v-if="currentTreeNode.url" class="mr-45">
                  <span style="font-size: 16px; cursor: pointer">
                    更多
                    <a-icon
                      type="more"
                      class="text-muted"
                      style="font-size: 16px"
                    />
                  </span>
                  <template #overlay>
                    <a-menu slot="overlay" @click="handleMenuClick">
                      <a-menu-item key="1" v-if="currentFileDetial">
                        <a-icon type="eye" />
                        {{
                          currentFileDetial.listTree
                            ? "包"
                            : viewCodes
                            ? "文件"
                            : folibRepository.layout === "Docker"
                            ? "详情"
                            : ""
                        }}预览
                      </a-menu-item>
                      <a-menu-item
                        key="2"
                        v-if="copyEnabled"
                      >
                        <a-icon type="copy" />复制
                      </a-menu-item>
                      <a-menu-item
                        key="3"
                        v-if="moveEnabled"
                      >
                        <a-icon type="swap" />移动
                      </a-menu-item>
                      <a-menu-item key="4"
                      v-if="deleteEnabled">
                        <a-popconfirm
                          title="确定要删除吗？"
                          placement="topLeft"
                          okType="danger"
                          ok-text="确定"
                          cancel-text="取消"
                          @confirm="deletePackageHandle"
                        >
                          <a-icon type="delete" />删除
                        </a-popconfirm>
                      </a-menu-item>
                      <a-menu-item
                        key="5"
                        v-if="
                          folibRepository.type !== 'group' &&
                          currentTreeNode && currentTreeNode.type === 'file'
                        "
                      >
                        <a-icon type="database" />元数据
                      </a-menu-item>
                    </a-menu>
                  </template>
                </a-dropdown>
              </a-col>
            </a-row>
          </template>

          <a
            v-if="currentTreeNode.url && folibRepository.layout !== 'Docker'"
            class="text-dark"
            :href="
              currentTreeNode.url.search('http://localhost:38080/') !== -1
                ? currentTreeNode.url.replace(
                    'http://localhost:38080/',
                    baseUrl
                  )
                : currentTreeNode.url
            "
            target="_blank"
            >{{
              currentTreeNode.url.search("http://localhost:38080/") !== -1
                ? currentTreeNode.url.replace(
                    "http://localhost:38080/",
                    baseUrl
                  )
                : currentTreeNode.url
            }}</a
          >

          <hr class="my-25" />
          <BaseData
            ref="BaseData"
            :currentTreeNode="currentTreeNode"
            :repositoryType="repositoryType"
            :currentFileDetial="currentFileDetial"
            :successMsg="successMsg"
            :folibRepository="folibRepository"
                  @metadataEditHandler="metadataEditHandler"

          />
        </a-card>
      </a-col>
    </a-row>
    <a-row v-if="isNotSearch === true" type="flex" :gutter="24">
      <!-- Platform Settings Column -->
      <a-col :span="24" :md="24" class="mb-24">
        <a-card
          :bordered="false"
          style="max-height: 1024px; min-height: 454px; overflow-y: auto"
          class="header-solid"
          :bodyStyle="{ paddingTop: 0, paddingBottom: 0 }"
        >
          <div class="mx-25">
            <a-row type="flex" :gutter="24">
              <a-col :span="24" md="12">
                <label for="" class="ml-10">显示数量</label>
                <a-select
                  class="ml-10 mt-10"
                  v-model="artifactQuery.limit"
                  @change="onPageSizeChange"
                  style="width: 70px"
                >
                  <a-select-option :value="5">5</a-select-option>
                  <a-select-option :value="10">10</a-select-option>
                  <a-select-option :value="15">15</a-select-option>
                  <a-select-option :value="20">20</a-select-option>
                  <a-select-option :value="25">25</a-select-option>
                </a-select>
                <a-config-provider
                  class="ml-10 mt-10"
                  :locale="locale"
                  style="width: 290px"
                >
                  <a-range-picker
                    :show-time="{ placeholder: '选择时间', format: 'HH:mm' }"
                    format="YYYY-MM-DD HH:mm"
                    :placeholder="['开始日期', '结束日期']"
                    @change="dateChange"
                    @ok="dateConfirm"
                  />
                </a-config-provider>
              </a-col>
              <a-col :span="24" md="12"> </a-col>
            </a-row>
          </div>
          <template #title>
            <h6 class="font-semibold m-0">搜索列表</h6>
          </template>

          <a-table
            class="mt-20"
            :columns="columns"
            rowKey="url"
            :data-source="searchData"
            @change="handleTableChange"
            :pagination="{
              pageSize: artifactQuery.limit,
              current: artifactQuery.page,
              total: artifactQuery.total,
              showLessItems: true,
            }"
          >
            <template slot="path" slot-scope="text, record">
              <a>
                <div
                  class="table-avatar-info"
                  @click="searchDataHandle(record)"
                >
                  <a-avatar
                    shape="circle"
                    :size="24"
                    :src="
                      folibRepository.layout === 'Docker'
                        ? 'images/folib/docker-s.svg'
                        : 'images/folib/' + getFileType(record.path) + '.svg'
                    "
                  />
                  <div class="avatar-info search-column-path">
                    <p class="mb-0 text-dark">
                      {{ record.artifactPath }}
                    </p>
                  </div>
                </div>
              </a>
            </template>
            <template slot="sizeInBytes" slot-scope="sizeInBytes">{{
              fileSizeConver(sizeInBytes)
            }}</template>
          </a-table>
        </a-card>
      </a-col>
    </a-row>
    <use-doc
      :usedVisible="usedVisible"
      :repositoryType="repositoryType"
      :folibRepository="folibRepository"
      :ivyCode="ivyCode"
      :baseUrl="baseUrl"
      :dockerCode="dockerCode"
      @close="closeUsedVisibleDialog"
    />
    <!-- 预览 -->
    <a-drawer
      placement="right"
      width="45%"
      :title="currentTreeNode.name"
      :visible="viewCodeVisible"
      @close="closeViewCodeDialog"
    >
      <div class="mx-auto m-50">
        <div class="mb-50">
          <a-card :bordered="false" class="header-solid">
            <a-directory-tree
              v-if="currentFileDetial && currentFileDetial.listTree"
              :replaceFields="{ title: 'name', children: 'children' }"
              :tree-data="currentFileDetial.listTree"
            />
          </a-card>
          <prism-editor
            class="my-editor height-300"
            v-if="
              currentFileDetial &&
              viewCodes &&
              folibRepository.layout !== 'Docker'
            "
            v-model="viewCodes"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>

          <a-tabs
            v-if="
              currentFileDetial &&
              currentManifest &&
              folibRepository.layout === 'Docker'
            "
            class="tabs-sliding"
            default-active-key="1"
          >
            <a-tab-pane key="1" tab="Layers">
              <a-timeline>
                <a-timeline-item
                  color="primary"
                  v-for="(key, index) in currentManifest.config"
                  :key="index"
                >
                  {{ index }}
                  <p>
                    {{ currentManifest.config[index] }}
                  </p>
                </a-timeline-item>
              </a-timeline>
            </a-tab-pane>
            <a-tab-pane key="2" tab="制作历史">
              <a-timeline>
                <a-timeline-item
                  color="primary"
                  v-for="(key, index) in currentManifest.history"
                  :key="index"
                >
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
    <add-metadata
      v-if="showMetadataHandler"
      :showMetadataHandler="showMetadataHandler"
      :quillOptions="quillOptions"
      :handlerMetadataType="handlerMetadataType"
      :propMetadataForm="metadataForm"
      :metadataConfigList="metadataConfigList"
      :currentTreeNode="currentTreeNode"
      :metadataTypes="metadataTypes"
      :successMsg="successMsg"
      @metadataHandlerCancel="metadataHandlerCancel"
      @metadataReflesh="metadataReflesh"
    />

    <!-- 复制 -->
    <a-modal
      v-model="showOperationFormModal"
      :footer="null"
      :forceRender="true"
      :centered="true"
      :title="operationTitle"
      on-ok="showCopyFormModal = false"
    >
      <a-form
        :form="operationForm"
        ref="operationForm"
        layout="vertical"
        @submit.prevent="handleOperationSubmit"
      >
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-item
              class="tags-field mb-10"
              label="目标仓库"
              :colon="false"
              ref="targetRepositories"
              prop="targetRepositories"
            >
              <gb-ant-select-two-cascader
                allowClear
                placeholder="请选择目标仓库"
                v-decorator="[
                  'targetRepositories',
                  {
                    initialValue: [],
                    rules: [
                      {
                        required: true,
                        message: '请选择目标仓库',
                        type: 'array',
                      },
                    ],
                  },
                ]"
                :selectOptionsConfig="{
                  key: 'key',
                  value: 'key',
                  text: 'name',
                  children: 'children',
                }"
                dropdownClassName="customer-multiple-cascader"
                :treeData="repositories"
              />
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
            <a-form-item
              class="tags-field mb-10"
              v-if="!custom"
              label="目标目录"
              prop="path"
              :colon="false"
            >
              <a-input
                v-decorator="[
                  'path',
                  {
                    rules: [{ required: true, message: '请输入目标目录' }],
                  },
                ]"
                :disabled="true"
                placeholder="请输入目标目录"
              >
              </a-input>
            </a-form-item>
            <a-form-item
              class="tags-field mb-10"
              v-if="custom"
              label="目标目录"
              prop="path"
              :colon="false"
            >
              <a-input
                v-decorator="[
                  'path',
                  {
                    rules: [{ required: true, message: '请输入目标目录' }],
                  },
                ]"
                :disabled="false"
                placeholder="请输入目标目录"
              >
              </a-input>
            </a-form-item>
          </a-col>
          <a-col :span="24" class="text-center">
            <a-button
              key="submit"
              class="px-30"
              size="small"
              type="primary"
              htmlType="submit"
              >提交</a-button
            >
            <a-button
              key="back"
              @click="operationFormModalClose()"
              class="px-30 ml-10"
              size="small"
              >取消</a-button
            >
          </a-col>
        </a-row>
      </a-form>
    </a-modal>
    <!--    rpm 上传表单 start-->
    <a-modal
      v-model="showRpmUploadFormModal"
      :footer="null"
      :forceRender="true"
      :centered="true"
      title="上传"
      on-ok="showRpmUploadFormModal = false"
    >
      <a-form
        :form="rpmUploadForm"
        ref="rpmUploadForm"
        layout="horizontal"
        @submit.prevent="handleRpmUploadSubmit"
      >
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-item
              class="tags-field mb-10"
              label="目标仓库"
              prop="repostoryId"
              :colon="false"
            >
              <a-input
                v-decorator="[
                  'repostoryId',
                  {
                    rules: [{ required: true, message: '请输入目标仓库' }],
                  },
                ]"
                :disabled="true"
                placeholder="请输入目标仓库"
              >
              </a-input>
            </a-form-item>
            <a-form-item label="选择文件">
              <a-upload
                v-decorator="[
                  'files',
                  {
                    rules: [{ required: true, message: '请选择文件' }],
                    valuePropName: 'fileList',
                    getValueFromEvent: normFile,
                  },
                ]"
                name="files"
                :multiple="true"
                :beforeUpload="beforeUpload"
                list-type="text"
                accept=".rpm"
              >
                <a-button> <a-icon type="upload" />选择文件 </a-button>
              </a-upload>
            </a-form-item>
          </a-col>
          <a-col :span="24" class="text-center">
            <a-button
              key="submit"
              class="px-30"
              size="small"
              type="primary"
              htmlType="submit"
              >上传</a-button
            >
            <a-button
              key="back"
              @click="uploadRpmFormModalClose()"
              class="px-30 ml-10"
              size="small"
              >取消</a-button
            >
          </a-col>
        </a-row>
      </a-form>
    </a-modal>
    <!--   rpm 上传表单 end -->
    <!-- raw 、maven、npm 上传 -->
    <a-modal
      v-model="showUploadFormModal"
      :footer="null"
      :forceRender="true"
      :centered="true"
      title="上传"
      on-ok="showUploadFormModal = false"
    >
      <a-form
        :form="uploadForm"
        ref="uploadForm"
        layout="horizontal"
        @submit.prevent="handleUploadSubmit"
      >
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-item
              class="tags-field mb-10"
              label="目标仓库"
              prop="repostoryId"
              :colon="false"
            >
              <a-input
                v-decorator="[
                  'repostoryId',
                  {
                    rules: [{ required: true, message: '请输入目标仓库' }],
                  },
                ]"
                :disabled="true"
                placeholder="请输入目标仓库"
              >
              </a-input>
            </a-form-item>
            <a-form-item label="选择文件">
              <a-upload v-decorator="[
                'files',
                {
                  rules: [{ required: true, message: '请选择文件' }],
                  valuePropName: 'fileList',
                  getValueFromEvent: normFile,
                },
              ]" name="files" :multiple="true" :beforeUpload="beforeUpload" list-type="text"
              :accept="folibRepository.layout === 'Raw'?'*':folibRepository.layout === 'npm'?'.tgz':'.jar,.war,.pom'">
                <a-button> <a-icon type="upload" />选择文件 </a-button>
              </a-upload>
            </a-form-item>
            <a-form-item class="tags-field mb-10" label="目标目录" prop="targetPath" :colon="false"
              v-if="folibRepository.layout !== 'Maven 2' && folibRepository.layout !== 'npm'">
              <a-input v-decorator="[
                'targetPath',
                {
                  rules: [
                    { required: false, message: '请输入目标目录' }
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
  </div>
</template>

<script>
import storage from "store";
import uuidv4 from "uuid/v4"
import {
  getLayoutType,
  getFileType,
  fileSizeConver,
  formateDate,
} from "@/utils/layoutUtil";
import {
  browse,
  getArtifact,
  viewArtifactFile,
  fql,
  scannerRules,
  insertOrUpdateRules,
  getDockerArtifact,
  deleteArtifact,
  repositoryVulnerabilityStatistics,
  getPermissionStoragesAndRepositories,
  getStorageAndRepositoryPermission,
} from "@/api/folib";
import {
  artifactCopy,
  artifactMove,
  artifactUpload,
  artifactUploadProgress,
  rpmArtifactUpload,
} from "@/api/artifact";
import { getMetadataConfiguration } from "@/api/settings";
import { hasRole, isAdmin, hasPermission } from "@/utils/permission";


import SearchBox from "@/components/Tools/SearchBox";
import zhCN from "ant-design-vue/es/locale/zh_CN";

import BaseData from "./Data.vue";
import UseDoc from "./UseDoc.vue";
import AddMetadata from "./AddMetadata.vue";

import { PrismEditor } from "vue-prism-editor";
import "vue-prism-editor/dist/prismeditor.min.css"; // import the styles somewhere
// import highlighting library (you can use any library you want just return html string)
import { highlight, languages } from "prismjs/components/prism-core";
import "prismjs/components/prism-clike";
import "prismjs/components/prism-javascript";
import "prismjs/themes/prism-tomorrow.css";
export default {
  inject: ["reload"],
  props: [
    "metadataTypes",
    "quillOptions",
    "successMsg",
    "searchType",
    "propScanReport",
    "formateDate",
  ],
  components: {
    PrismEditor,
    SearchBox,
    BaseData,
    UseDoc,
    AddMetadata,
  },
  data() {
    return {
      baseUrl: "",
      folibRepository: {},
      repositoryType: null,
      rpmUploadForm: this.$form.createForm(this, { name: "rpmUpload_form" }),
      uploadForm: this.$form.createForm(this, { name: "upload_form" }),
      showUploadFormModal: false,
      showRpmUploadFormModal: false,
      uploadEnabled: false,
      copyEnabled: false,
      moveEnabled: false,
      deleteEnabled: false,
      scan: {
        id: "",
        repository: "",
        storage: "",
        onScan: false,
        scanRule: null,
        layout: null,
      },
      treeData: [],
      currentFileDetial: null,
      currentTreeNode: {},
      detialVisible: false,

      metadataList: [],
      metadataConfigList: [],
      metadataEditorDrawerTitle: undefined,
      metadataEditorDrawerVisible: false,
      metadataEditorDrawerValue: undefined,
      metadataPrismEditorDrawerTitle: undefined,
      metadataPrismEditorDrawerValue: false,
      metadataPrismEditorDrawerVisible: undefined,
      handlerMetadataType: 1,
      showMetadataHandler: false,
      metadataForm: {
        key: undefined,
        customKey: undefined,
        custom: false,
        type: undefined,
        viewShow: true,
        value: undefined,
      },
      metadataInput: true,
      metadataNumber: false,
      metadataEditor: false,
      prismEditor: false,
      codeParam: {
        type: "",
        code: null,
      },
      artifactQuery: {
        artifactName: null,
        metadataSearch: null,
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
      // 用户帮助
      usedVisible: false,
      ivyCode: null,
      dockerCode: { ubuntu: null, centos: null, windows: null, macos: null },
      // 预览
      operationForm: this.$form.createForm(this, { name: "operation_form" }),
      viewCodeVisible: false,
      viewCodes: null,
      locale: zhCN,
      isNotSearch: false,
      searchData: [],
      searchDataCurrentSelect: {},
      searchViewCodeVisible: false,
      searchViewCodes: null,
      columns: [
        {
          title: "所属仓库",
          dataIndex: "repositoryId",
          scopedSlots: { customRender: "repositoryId" },
          width: 150,
        },
        {
          title: "制品路径",
          dataIndex: "path",
          scopedSlots: { customRender: "path" },
          width: 550,
        },
        {
          title: "创建时间",
          dataIndex: "created",
          sorter: true,
          sortDirections: ["descend", "ascend"],
          scopedSlots: { customRender: "created" },
          width: 200,
        },
        {
          title: "最近使用时间",
          dataIndex: "lastUsed",
          sorter: true,
          scopedSlots: { customRender: "lastUsed" },
          width: 200,
        },
        {
          title: "下载次数",
          dataIndex: "downloadCount",
          sorter: true,
          scopedSlots: { customRender: "created" },
          width: 200,
        },
        {
          title: "制品大小",
          dataIndex: "sizeInBytes",
          sorter: true,
          scopedSlots: { customRender: "sizeInBytes" },
          width: 200,
        },
      ],
      scanReport: {
        show: false,
        report: [],
        vulnerabilitesCount: 0,
        critical: 0,
        high: 0,
        medium: 0,
        low: 0,
      },
      operationTitle: "",
      showOperationFormModal: false,
      repositories: [],
      custom: false,
      enablUploadedLayout: ['Raw', 'php', 'Maven 2', 'npm'],
      permissions: []
    };
  },
  created() {
    this.createData();
    this.getBrowse();
    this.scannerRules();
    this.repositoryVulnerabilityStatistics();
    this.scanReport = Object.assign({}, this.propScanReport);
    this.queryStorageAndRepositoryPermission();
  },
  methods: {
    scannerRules() {
      scannerRules(
        this.folibRepository.storageId + "-" + this.folibRepository.id
      ).then((res) => {
        if (res.rel) {
          this.scan = res.data;
        }
      });
    },
    scannerChange() {
      this.scan.id =
        this.folibRepository.storageId + "-" + this.folibRepository.id;
      this.scan.repository = this.folibRepository.id;
      this.scan.storage = this.folibRepository.storageId;
      this.scan.layout = this.folibRepository.layout;
      insertOrUpdateRules(this.scan).then((res) => {
        setTimeout(() => {
          this.$notification.success({
            message: this.scan.onScan ? "开启扫描" : "关闭扫描",
          });
        }, 100);
      });
    },
    repositoryVulnerabilityStatistics() {
      repositoryVulnerabilityStatistics({
        storageId: this.folibRepository.storageId,
        repositoryId: this.folibRepository.id,
      }).then((res) => {
        this.vulnerabilityStatistics = res;
      });
    },
    goBack() {
      this.$router.push({ name: "storages" });
    },
    getLayoutTypeHandle() {
      return getLayoutType(this.folibRepository);
    },
    getBrowse() {
      if (this.folibRepository.status.indexOf('Out of Service') !== -1) {
        this.$notification.warning({
          message: "该仓库已关闭服务",
        })
        return false
      }
      if (!this.folibRepository.allowsDirectoryBrowsing) {
        this.$notification.warning({
          message: "该仓库目录浏览未开启",
        })
        return false
      }
      browse(this.folibRepository.storageId, this.folibRepository.id, "")
        .then((res) => {
          const d = res.directories;
          d.forEach((item, index, d) => {
            item.type = "dir";
          });
          const f = res.files;
          f.forEach((item, index) => {
            item.isLeaf = true;
            item.type = "file";
          });
          this.treeData = d.concat(f);
        })
        .catch((err) => {
        });
    },
    createData() {
      //上个页面通过缓存传参，目的防止页面刷新，路由数据消失
      const params = storage.get("libView_repository");
      this.folibRepository = params.item;
      this.baseUrl = params.baseUrl;
      this.repositoryType = this.getLayoutTypeHandle();
    },
    copy(url) {
      var input = document.createElement("input"); // 创建input对象
      input.value = url; // 设置复制内容
      document.body.appendChild(input); // 添加临时实例
      input.select(); // 选择实例内容
      document.execCommand("Copy"); // 执行复制
      document.body.removeChild(input); // 删除临时实例
      // console.log(url)
      setTimeout(() => {
        this.$notification.success({
          message: "复制成功",
        });
      }, 100);
    },
    handleRpmUpload() {
      this.rpmUploadForm.resetFields();
      this.$nextTick(() => {
        if (this.$refs.rpmUploadForm) {
          this.rpmUploadForm.setFieldsValue({
            repostoryId: this.folibRepository.id,
          });
        }
      });
      this.showRpmUploadFormModal = true;
    },
    uploadRpmFormModalClose() {
      this.rpmUploadForm.resetFields();
      this.showRpmUploadFormModal = false
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
    handleUpload() {
      this.uploadForm.resetFields();
      this.$nextTick(() => {
        if (this.$refs.uploadForm) {
          this.uploadForm.setFieldsValue({
            repostoryId: this.folibRepository.id,
          });
        }
      });
      this.showUploadFormModal = true;
    },
    handleRpmUploadSubmit(e) {
      e.preventDefault()
      this.rpmUploadForm.validateFields((err, values) => {
        if (!err) {
          if (values.files.length > 10) {
            this.$notification["warning"]({
              message: "一次上传不能超过10个文件",
              description: "",
            });
            return false
          }
          let fileList = []
          for(let item of values.files){
            let fileName = item.name.replace(":", "/")
            if (!this.check(fileName, item.size)) {
              return false
            }
            item.name = fileName
            fileList.push(item)
          }
          fileList.forEach(item => {
            this.handlerRpmUploadFile(values.targetPath, item.name.replace(":", "/"), item.originFileObj)
          })
          this.successMsg("请至页面右上角上传进度中查看")
          this.uploadRpmFormModalClose()
        }
      });
    },
    handlerRpmUploadFile(targetPath, fileName, file) {
      file = new File([file], fileName)
      let filePathMap = {}
      filePathMap[fileName] = targetPath ? targetPath + "/" + fileName : fileName
      let uuid = uuidv4()
      const formData = new FormData()
      formData.append("storageId", this.folibRepository.storageId)
      formData.append("repostoryId", this.folibRepository.id)
      formData.append("filePathMap", JSON.stringify(filePathMap))
      formData.append("files", file)
      rpmArtifactUpload( this.folibRepository.storageId, this.folibRepository.id, formData, uuid, fileName).then((res) => {
      }).catch((err) => {
        let msg = err.response.data.error?err.response.data.error:err.response.data
        console.log('rpm upload error：', msg)
        let errStatusArr = [200, 500]
        if(!errStatusArr.includes(err.response.status)) {
            this.$notification["error"]({
              message: '错误编码：' + err.response.status,
              description: "",
            });
        }
      }).finally(() => { 
      })
    },
    handleUploadSubmit(e) {
      e.preventDefault()
      this.uploadForm.validateFields((err, values) => {
        if (!err) {
          if (values.files.length > 10) {
            this.$notification["warning"]({
              message: "一次上传不能超过10个文件",
              description: "",
            });
            return false
          }
          if (values.targetPath && values.targetPath.startsWith("/")) {
            this.$notification["warning"]({
              message: "目标目录不能以/开头",
              description: "",
            });
            return false
          }
          let fileList = []
          for(let item of values.files){
            let fileName = item.name.replace(":", "/")
            if (!this.check(fileName, item.size)) {
              return false
            }
            item.name = fileName
            fileList.push(item)
          }
          fileList.forEach(item => {
            this.handlerUploadFile(values.targetPath, item.name, item.originFileObj)
          })
          this.successMsg("请至页面右上角上传进度中查看")
          this.uploadFormModalClose()
        }
      })
    },
    check(fileName, fileSize) {
      let layout = this.folibRepository.layout
      if (layout === 'Maven 2') {
        let policy = this.folibRepository.policy
        let regex = /^(.*)-([0-9]{8}.[0-9]{6})-([0-9]+)(.*)$/
        let isSnapshot = fileName.indexOf('SNAPSHOT') !== -1 || regex.test(fileName)
        let msg = null
        if (policy === 'release' && isSnapshot) {
          msg = fileName + '为snapshot版本，仓库版本策略为release，禁止上传'
        }
        if (policy === 'snapshot' && !isSnapshot) {
          msg = fileName + '为snapshot版本，仓库版本策略为release，禁止上传'
        }
        if (msg) {
          this.$notification["warning"]({
            message: msg,
            description: ""
          })
          return false
        }
      }
      let fileSizeLimit = 2 * 1024 * 1024 * 1024
      if (fileSize > fileSizeLimit) {
        this.$notification["warning"]({
          message: fileName + "超过2G，禁止上传",
          description: ''
        })
        return false
      }
      return true
    },
    handlerUploadFile(targetPath, fileName, file) {
      file = new File([file], fileName)
      let filePathMap = {}
      filePathMap[fileName] = targetPath ? targetPath + "/" + fileName : fileName
      let uuid = uuidv4()
      const formData = new FormData()
      formData.append("storageId", this.folibRepository.storageId)
      formData.append("repostoryId", this.folibRepository.id)
      formData.append("filePathMap", JSON.stringify(filePathMap))
      formData.append("files", file)
      artifactUploadProgress(formData, uuid, fileName).then((res) => {
      }).catch((err) => {
        let msg = err.response.data.error?err.response.data.error:err.response.data
        console.log('upload error：', msg)
        let errStatusArr = [200,500]
        if(!errStatusArr.includes(err.response.status)) {
            this.$notification["error"]({
              message: '错误编码：' + err.response.status,
              description: "",
            });
        }
      }).finally(() => { 
      })
    },
    uploadFormModalClose() {
      this.showUploadFormModal = false;
    },
    UsedHelperVisible() {
      if (this.repositoryType === "ivy") {
        this.ivyCode =
          "<ivysettings>\n" +
          '   <settings defaultResolver="' +
          this.folibRepository.id +
          '" defaultConflictManager="all" />\n' +
          "   <resolvers>\n" +
          '        <ibiblio name="releases" root="' +
          this.baseUrl +
          "storages/" +
          this.folibRepository.storageId +
          "/" +
          this.folibRepository.id +
          '" m2compatible="true" usepoms="true"/>\n' +
          "   </resolvers>\n" +
          "</ivysettings>";
      } else if (this.repositoryType === "docker") {
        this.dockerCode.ubuntu =
          "sudo mkdir -p /etc/docker\n" +
          "sudo tee /etc/docker/daemon.json <<-'EOF'\n" +
          "{\n" +
          '"insecure-registries": ["' +
          this.baseUrl.replace("http://", "").replace("/", "") +
          '"]\n' +
          "}\n" +
          "EOF\n" +
          "sudo systemctl daemon-reload\n" +
          "sudo systemctl restart docker";
        this.dockerCode.centos =
          "sudo mkdir -p /etc/docker\n" +
          "sudo tee /etc/docker/daemon.json <<-'EOF'\n" +
          "{\n" +
          '"insecure-registries": ["' +
          this.baseUrl.replace("http://", "").replace("/", "") +
          '"]\n' +
          "}\n" +
          "EOF\n" +
          "sudo systemctl daemon-reload\n" +
          "sudo systemctl restart docker";
        this.dockerCode.macos = this.baseUrl;
        this.dockerCode.windows =
          "{\n" +
          '  "insecure-registries": ["' +
          this.baseUrl.replace("http://", "").replace("/", "") +
          '"]\n' +
          "}";
      }
      this.usedVisible = true;
    },
    scannerChange() {
      this.scan.id =
        this.folibRepository.storageId + "-" + this.folibRepository.id;
      this.scan.repository = this.folibRepository.id;
      this.scan.storage = this.folibRepository.storageId;
      this.scan.layout = this.folibRepository.layout;
      insertOrUpdateRules(this.scan).then((res) => {
        setTimeout(() => {
          this.$notification.success({
            message: this.scan.onScan ? "开启扫描" : "关闭扫描",
          });
        }, 100);
      });
    },
    onLoadData(treeNode) {
      this.currentFileDetial = null;
      if (this.folibRepository.layout === "Docker") {
        return new Promise((resolve) => {
          if (treeNode.dataRef.children) {
            resolve();
            return;
          }
          getDockerArtifact(
            this.folibRepository.storageId,
            this.folibRepository.id,
            treeNode.dataRef.artifactPath
          ).then((res) => {
            if (res.directories.length > 0) {
              const d = res.directories;
              d.forEach((item, index, d) => {
                item.type = "dir";
              });
              treeNode.dataRef.children = d;
            } else if (res.files.length > 0) {
              const a = res.files;
              a.forEach((item, index, a) => {
                item.isLeaf = true;
                item.type = "file";
              });
              treeNode.dataRef.children = a;
            }

            this.treeData = [...this.treeData];
            resolve();
          });
        });
      }

      return new Promise((resolve) => {
        if (treeNode.dataRef.children) {
          resolve();
          return;
        }
        browse(
          this.folibRepository.storageId,
          this.folibRepository.id,
          treeNode.dataRef.artifactPath
        ).then((res) => {
          if (!treeNode.dataRef.children) {
            treeNode.dataRef.children = [];
          }
          if (res.directories.length > 0) {
            const d = res.directories;
            d.forEach((item, index, d) => {
              item.type = "dir";
            });
            treeNode.dataRef.children = d;
          }
          if (res.files.length > 0) {
            const a = res.files;
            a.forEach((item, index, a) => {
              item.isLeaf = true;
              item.type = "file";
            });
            treeNode.dataRef.children = treeNode.dataRef.children.concat(a);
          }

          this.treeData = [...this.treeData];
          resolve();
        });
      });
    },
    treeSelect(key, e) {
      this.currentTreeNode = e.node.dataRef;
      this.scanReport = {
        show: false,
        report: [],
        vulnerabilitesCount: 0,
        critical: 0,
        high: 0,
        medium: 0,
        low: 0,
      };
      if (this.currentTreeNode.type === "file") {
        getArtifact(
          this.repositoryType,
          this.currentTreeNode.storageId,
          this.currentTreeNode.repositoryId,
          this.currentTreeNode.artifactPath
        ).then((res) => {
          this.currentFileDetial = res;
          if (this.currentFileDetial.snippets) {
            this.changeCodeTye(this.currentFileDetial.snippets[0]);
          }
          if (this.currentFileDetial.artifact) {
            if (this.currentFileDetial.artifact.safeLevel === "scanComplete") {
              this.scanReport.show = true
              this.scanReport.vulnerabilitesCount =
                this.currentFileDetial.artifact.vulnerabilitiesCount;
              this.scanReport.critical =
                this.currentFileDetial.artifact.criticalVulnerabilitiesCount;
              this.scanReport.high =
                this.currentFileDetial.artifact.highVulnerabilitiesCount;
              this.scanReport.medium =
                this.currentFileDetial.artifact.mediumVulnerabilitiesCount;
              this.scanReport.low =
                this.currentFileDetial.artifact.lowVulnerabilitiesCount;
              this.scanReport.report = JSON.parse(
                this.currentFileDetial.artifact.report
              )
            }
          }
          this.currentManifest = res.manifestConfig;
          this.handlerRespMetadata(res);
        });
      } else if (this.currentTreeNode.type === "dir") {
        this.currentFileDetial = null;
      }
    },
    handleMenuClick(active) {
      this.operationForm.resetFields();
      this.$nextTick(() => {
        if (this.$refs.operationForm) {
          this.operationForm.setFieldsValue({
            path: this.currentTreeNode.artifactPath,
          });
        }
      });
      if (active.key === "1") {
        this.viewCodeHandle();
      } else if (active.key === "2" || active.key === "3") {
        //复制 或 移动
        this.showOperationFormModal = true;
        this.queryPermissionStoragesAndRepositories(
          this.folibRepository.type,
          this.folibRepository.layout,
          this.folibRepository.id,
          this.folibRepository.policy
        );
        this.operationTitle =
          active.key === "2"
            ? "复制 " + this.currentTreeNode.artifactPath
            : "移动  " + this.currentTreeNode.artifactPath;
        this.customTitle =
          active.key === "2" ? "复制到自定义目录" : "移动到自定义目录";
      } else if (active.key === "4") {
        //删除
      } else if (active.key === "5") {
        //元数据
        this.getMetadataConfiguration();
        this.metadataHandler(1);
      }
    },
    handleOperationSubmit(e) {
      e.preventDefault();
      this.operationForm.validateFields((err, values) => {
        if (!err) {
          let targetRepositoyList = [];
          values.targetRepositories.forEach((item) => {
            let split = item.split(",");
            targetRepositoyList.push({
              targetStorageId: split[0],
              targetRepositoryId: split[1],
            });
          });
          let data = {
            path: values.path,
            srcStorageId: this.folibRepository.storageId,
            srcRepositoryId: this.folibRepository.id,
            targetRepositoyList: targetRepositoyList,
          };
          if (this.operationTitle.indexOf("复制") !== -1) {
            artifactCopy(data)
              .then((res) => {
                this.successMsg("复制中，请稍候查看");
                this.operationFormModalClose();
                this.reload();
              })
              .catch((err) => {
                this.$notification["error"]({
                  message: err.response.data.error,
                  description: "",
                });
              })
              .finally(() => { });
          } else if (this.operationTitle.indexOf("移动") !== -1) {
            artifactMove(data)
              .then((res) => {
                this.successMsg("移动中，请稍候查看");
                this.operationFormModalClose();
                this.reload();
              })
              .catch((err) => {
                this.$notification["error"]({
                  message: err.response.data.error,
                  description: "",
                });
              })
              .finally(() => { });
          }
        }
      });
    },
    operationFormModalClose() {
      this.showOperationFormModal = false;
    },
    queryPermissionStoragesAndRepositories(type, layout, excludeRepositoryId, policy) {
      getPermissionStoragesAndRepositories({
        type: type,
        layout: layout,
        excludeRepositoryId: excludeRepositoryId,
        policy: policy,
      }).then((res) => {
        this.repositories = [];
        res.forEach((item) => {
          if (item.children && item.children.length > 0) {
            this.repositories.push(item);
          }
        });
      });
    },
    getMetadataConfiguration() {
      getMetadataConfiguration()
        .then((res) => {
          this.metadataConfigList = res;
        })
        .finally(() => { });
    },
    metadataHandler(type, metadata) {
      this.metadataFormReset();
      if (metadata) {
        this.metadataForm = metadata;
      }
      this.handlerMetadataType = type;
      this.showMetadataHandler = true;
    },
    metadataFormReset() {
      if (this.$refs.metadataForm) {
        this.$refs.metadataForm.resetFields();
      }
      this.metadataForm = {
        key: undefined,
        customKey: undefined,
        custom: false,
        type: undefined,
        viewShow: true,
        value: undefined,
      };
      this.metadataInput = true;
      this.metadataEditor = false;
      this.metadataNumber = false;
      this.prismEditor = false;
    },
    deletePackageHandle() {
      deleteArtifact(
        this.currentTreeNode.storageId,
        this.currentTreeNode.repositoryId,
        this.currentTreeNode.artifactPath
      )
        .then((res) => {
          setTimeout(() => {
            this.$notification.success({
              message: "删除成功",
            });
            this.reload();
          }, 100);
        })
        .catch((err) => {
          let msg = err.response.data.message?err.response.data.message:err.response.data.error?err.response.data.error:err.response.data
          if (!msg || msg.length === 0 || typeof(msg) === "object") {
            msg = "删除失败"
          }
          this.$notification.error({
            message: msg,
            description: ""
          })
        })
        .finally(() => { });
    },
    handlerRespMetadata(res) {
      let metadataList = [];
      if (
        res.artifact &&
        res.artifact.metadata &&
        res.artifact.metadata.length > 0
      ) {
        let metadataJson = JSON.parse(res.artifact.metadata);
        for (let key in metadataJson) {
          let flag = this.metadataConfigList.some(
            (metadataConfig) =>
              !metadataConfig.viewShow && metadataConfig.key === key
          );
          if (flag) {
            metadataJson[key].viewShow = false;
          }
          let metadata = Object.assign({}, metadataJson[key]);
          metadata.key = key;
          metadataList.push(metadata);
        }
      }
      this.metadataList = metadataList;
      this.$forceUpdate();
    },
    metadataEditorDrawerShow(metadata) {
      this.metadataEditorDrawerTitle = metadata.key;
      this.metadataEditorDrawerValue = metadata.value;
      this.metadataEditorDrawerVisible = true;
    },
    metadataEditHandler(metadata) {
      let key = metadata.key;
      let data = {
        key: undefined,
        customKey: undefined,
        custom: false,
        type: metadata.type,
        viewShow: metadata.viewShow === 1,
        value: metadata.value,
      };
      let flag = this.metadataConfigList.some((item) => item.key === key);
      if (!flag) {
        data.custom = true;
        data.customKey = key;
      } else {
        data.key = key;
        data.custom = false;
      }
      this.metadataHandler(2, data);
      this.metadataTypeChange(data.type);
    },
    metadataTypeChange(value) {
      let editorList = ["TEXT", "MD"];
      let prismEditorList = ["JSON"];
      let numberList = ["NUMERICAL"];
      if (editorList.indexOf(value) !== -1) {
        this.metadataEditor = true;
        this.metadataInput = false;
        this.metadataNumber = false;
        this.prismEditor = false;
      } else if (prismEditorList.indexOf(value) !== -1) {
        this.prismEditor = true;
        this.metadataInput = false;
        this.metadataNumber = false;
        this.metadataEditor = false;
      } else if (numberList.indexOf(value) !== -1) {
        if (this.handlerMetadataType === 1) {
          this.metadataForm.value = undefined;
        }
        this.metadataNumber = true;
        this.metadataInput = false;
        this.prismEditor = false;
        this.metadataEditor = false;
      } else {
        this.metadataInput = true;
        this.metadataEditor = false;
        this.metadataNumber = false;
        this.prismEditor = false;
      }
    },
    metadataPrismEditorDrawerShow(metadata) {
      this.metadataPrismEditorDrawerTitle = metadata.key;
      this.metadataPrismEditorDrawerValue = metadata.value;
      this.metadataPrismEditorDrawerVisible = true;
    },
    changeCodeTye(item) {
      if (item) {
        this.codeParam = {
          type: item.name === "Maven 2" ? "maven" : item.name.toLowerCase(),
          code: item.code,
        };
      }
    },
    getFileType(name) {
      if (name) {
        return getFileType(name);
      }
    },
    closeUsedVisibleDialog() {
      this.usedVisible = false;
    },
    viewCodeHandle() {
      if (this.folibRepository.layout !== "Docker") {
        if (this.currentFileDetial && !this.currentFileDetial.listTree) {
          viewArtifactFile(this.currentTreeNode.url).then((res) => {
            this.viewCodes = res;
          });
        }
      } else {
        // this.viewCodes=this.currentManifest.config
      }

      this.viewCodeVisible = true;
    },
    closeViewCodeDialog() {
      this.viewCodeVisible = false;
      this.viewCodes = null;
    },
    metadataHandlerCancel() {
      this.metadataFormReset();
      this.showMetadataHandler = false;
    },
    metadataReflesh() {
      this.metadataFormReset();
      this.$refs.BaseData.getMetadata();
      this.showMetadataHandler = false;
    },
    search(value, page) {
      if (page) {
        this.artifactQuery.page = page;
      }
      if (value) {
        if (this.searchType === 1) {
          this.artifactQuery.artifactName = value;
          this.artifactQuery.metadataSearch = null;
        } else if (this.searchType === 2) {
          this.artifactQuery.metadataSearch = value;
          this.artifactQuery.artifactName = null;
        }
      }
      this.artifactQuery.storageId = this.folibRepository.storageId;
      this.artifactQuery.repositoryId = this.folibRepository.id;
      let params = {
        artifactName: this.artifactQuery.artifactName,
        metadataSearch: this.artifactQuery.metadataSearch,
        storageId: this.artifactQuery.storageId,
        repositoryId: this.artifactQuery.repositoryId,
        limit: this.artifactQuery.limit,
        page: this.artifactQuery.page,
        sortField: this.artifactQuery.sortField,
        sortOrder: this.artifactQuery.sortOrder,
        beginDate: this.artifactQuery.beginDate,
        endDate: this.artifactQuery.endDate,
        regex: false,
      };
      if (params.artifactName && this.folibRepository.layout === "Docker") {
        // params.regex = true;
        // params.artifactName =
        //   "(" +
        //   params.storageId +
        //   "-" +
        //   params.repositoryId +
        //   ")(.*" +
        //   params.artifactName +
        //   ".*)";
      }
      fql(params).then((res) => {
        this.searchData = res.artifact;
        this.artifactQuery.total = res.total;
      });
      this.isNotSearch = true;
    },
    onPageSizeChange() {
      this.search(this.artifactQuery.artifactName, 1);
    },
    handleTableChange(pagination, filters, sorter) {
      this.artifactQuery.sortField = null;
      this.artifactQuery.sortOrder = null;
      if (pagination) {
        this.artifactQuery.page = pagination.current;
      }
      if (sorter) {
        this.artifactQuery.sortField = sorter.field;
        if (sorter.order) {
          this.artifactQuery.sortOrder = "asc";
          if (sorter.order.indexOf("desc") !== -1) {
            this.artifactQuery.sortOrder = "desc";
          }
        }
      }
      this.search(this.artifactQuery.artifactName);
    },
    dateChange(value, dateString) {
      if (dateString) {
        this.artifactQuery.beginDate = dateString[0];
        this.artifactQuery.endDate = dateString[1];
        if (
          this.artifactQuery.beginDate === "" &&
          this.artifactQuery.endDate === ""
        ) {
          this.dateConfirm();
        }
      }
    },
    dateConfirm() {
      this.search(this.artifactQuery.artifactName, 1);
    },
    searchDataHandle(item) {
      this.$emit("searchDataHandle", item);
    },
    openDetial() {
      this.$emit("openDetial", this.scanReport);
    },
    highlighterHandle(code) {
      return highlight(code, languages.js); //returns html
    },
    fileSizeConver(size) {
      if (size) {
        return fileSizeConver(size);
      }
    },
    queryStorageAndRepositoryPermission() {
      this.permissions = []
      getStorageAndRepositoryPermission(this.folibRepository.storageId, this.folibRepository.id).then((res) => {
        this.permissions = res
        this.uploadEnabled = this.folibRepository.status.indexOf('Out of Service') === -1 && this.enablUploadedLayout.includes(this.folibRepository.layout) && this.folibRepository.type === 'hosted' && (hasRole('ARTIFACTS_MANAGER') || this.permissions.includes('ARTIFACTS_DEPLOY'))
        this.copyEnabled = this.folibRepository.type === 'hosted' && (hasRole('ARTIFACTS_MANAGER') || this.permissions.includes('ARTIFACTS_COPY'))
        this.moveEnabled = this.folibRepository.type === 'hosted' && (hasRole('ARTIFACTS_MANAGER') || this.permissions.includes('ARTIFACTS_MOVE'))
        this.deleteEnabled = this.folibRepository.type !== 'group' && (hasRole('ARTIFACTS_MANAGER') || this.permissions.includes('ARTIFACTS_DELETE'))
      })
    }
  },
};
</script>
