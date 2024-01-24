<template>
  <div class="repo-info">
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
                    <a-icon type="backward" :style="{
                      fontSize: '32px',
                      marginRight: '5px',
                      opacity: '0.8',
                      color: '#BFBFBFFF',
                    }" @click="goBack()" />
                  </a>
                  <a>
                    <a-avatar @click="createData" :size="84" shape="square"
                      style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
                      :src="'images/folib/' + getLayoutTypeHandle() + '.svg'" />
                  </a>
                  <div class="avatar-info">
                    <a-tooltip placement="topLeft">
                      <template slot="title">
                        {{ $t('Store.GotoBrowsePage') }}
                      </template>
                      <a :href="baseUrl +
                        'api/browse/' +
                        folibRepository.storageId +
                        '/' +
                        folibRepository.id" target="_blank">
                        <h4 class="font-semibold m-0" @click="createData">
                          {{ folibRepository.id }}
                        </h4>
                      </a>
                    </a-tooltip>
                    <a-descriptions title="" :column="1" class="repo-address">
                      <a-descriptions-item :label="$t('Store.BrowseAddress')">
                        <a-tooltip placement="topLeft">
                          <template slot="title">
                            {{ $t('Store.WarehouseBrowseAddress') }}
                          </template>
                          <a :href="baseUrl +
                            'api/browse/' +
                            folibRepository.storageId +
                            '/' +
                            folibRepository.id" target="_blank">
                            <p class="copy-p">
                              {{ baseUrl +
                              'api/browse/' +
                              folibRepository.storageId +
                              '/' +
                              folibRepository.id }}
                            </p>
                          </a>
                        </a-tooltip>
                        <a class="ml-10">
                          <a-icon type="copy" @click="
                            copy(
                              baseUrl +
                              'api/browse/' +
                              folibRepository.storageId +
                              '/' +
                              folibRepository.id
                            )" />
                        </a>
                      </a-descriptions-item>
                      <a-descriptions-item :label="$t('Store.UseAddress')">
                        <a-tooltip>
                          <template slot="title">
                            {{ $t('Store.WarehouseUsageAddress') }}
                          </template>
                          <a>
                            <p class="copy-p">
                              {{
                                getRepositoryUrl()
                              }}
                            </p>
                          </a>
                        </a-tooltip>
                        <a class="ml-10">
                          <a-icon type="copy" @click="
                          copy(
                            getRepositoryUrl()
                          )
                            " />
                        </a>
                      </a-descriptions-item>
                    </a-descriptions>
                  </div>
                </a-col>
                <a-col :span="24" :md="12" style="
                    display: flex;
                    align-items: center;
                    justify-content: flex-end;
                  ">
                  <a v-if="uploadEnabled && folibRepository.layout === 'rpm'">
                    <small style="padding-right: 20px" @click="handleRpmUpload">
                      {{ $t('Store.Upload') }}
                      <a-icon type="cloud-upload" />
                    </small>
                  </a>
                  <a v-if="uploadEnabled && folibRepository.layout === 'Maven 2'"><small style="padding-right: 20px"
                      @click="handleMavenUpload">
                    {{ $t('Store.Upload') }}
                      <a-icon type="cloud-upload" />
                    </small>
                  </a>
                  <a v-if="uploadEnabled && folibRepository.layout === 'go'"><small style="padding-right: 20px"
                      @click="handleMavenUpload">
                    {{ $t('Store.Upload') }}
                      <a-icon type="cloud-upload" />
                    </small>
                  </a>
                  <a v-if="uploadEnabled && folibRepository.layout !== 'rpm'"><small style="padding-right: 20px" @click="handleUpload">
                      ${{ t('BatchUpload') }}
                      <a-icon type="cloud-upload" />
                    </small>
                  </a>
                  <a v-if="folibRepository.layout !== 'Raw'">
                    <small style="padding-right: 20px" @click="UsedHelperVisible">
                      {{ $t('Store.UseHelp') }}
                      <a-icon type="question-circle" theme="filled" />
                    </small>
                  </a>
                  <div v-if="$store.state.user.token && folibRepository.type !== 'group'">
                    <span class="mr-15">{{
                      scan.onScan ? $t('Store.ScanOn') : $t('Store.ScanOff')
                    }}</span>
                    <a-switch default-checked v-model="scan.onScan" @change="scannerChange" />
                  </div>
                </a-col>
              </a-row>
            </template>
          </a-card>
        </a-col>
      </a-row>
    </a-affix>
    <a-row v-if="isSearch === false" type="flex" :gutter="24">
      <!-- Platform Settings Column -->
      <a-col :span="24" :md="10" class="mb-24">
        <a-card :bordered="false" style="max-height: 1024px; min-height: 454px; overflow-y: auto" class="header-solid"
          :bodyStyle="{ paddingTop: 0, paddingBottom: 0 }">
          <template #title>
            <h6 class="font-semibold m-0">{{ $t('Store.PacketList') }} <a class="ml-10" @click="reload()">
                <a-icon type="reload" /></a></h6>
          </template>
          <a-directory-tree :replaceFields="{
            key: 'artifactPath',
            title: 'name',
            children: 'children',
          }" :tree-data="treeData" :load-data="onLoadData" @select="treeSelect">
          </a-directory-tree>
        </a-card>
      </a-col>
      <a-col :span="24" :md="14" class="mb-24">
        <a-card :bordered="false" class="header-solid h-full card-profile-information"
          :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }" :headStyle="{ paddingRight: 0 }">
          <template #title>
            <a-row type="flex" align="middle" v-if="folibRepository.layout !== 'Docker'">
              <a-col :span="16" class="font-semibold m-0">
                <a-row type="flex" align="middle">
                  <a-col :span="8" :xs="24" :xl="16">
                    <a-avatar v-if="!currentTreeNode.isLeaf" :size="24" shape="square"
                      :src="'images/folib/package.svg'" />
                    <a-avatar v-if="currentTreeNode.isLeaf" :size="24" shape="square" :src="'images/folib/' +
                      getFileType(currentTreeNode.name) +
                      '.svg'
                      " />
                    {{ currentTreeNode.name }}
                  </a-col>
                  <a-col :span="8" :xs="24" :xl="8">
                    <span class="ml-auto" v-if="scanReport.show" @click="openDetial">
                      <a-space :size="1" class="avatar-chips">
                        <template v-if="scanReport.vulnerabilitesCount > 0">
                          <a-tooltip>
                            <template slot="title">{{ $t('Store.Seriousness') }}</template>
                            <div class="">
                              <a-avatar :size="24" :src="'images/folib/critical.svg'" />
                              <span class="mb-0 text-dark">{{
                                scanReport.critical
                              }}</span>
                            </div>
                          </a-tooltip>

                          <a-tooltip>
                            <template slot="title">{{ $t('Store.HighRisk') }}</template>
                            <div class="">
                              <a-avatar :size="24" :src="'images/folib/high.svg'" />
                              <span class="mb-0 text-dark">{{
                                scanReport.high
                              }}</span>
                            </div>
                          </a-tooltip>

                          <a-tooltip>
                            <template slot="title">{{ $t('Store.MediumRisk') }}</template>
                            <div class="">
                              <a-avatar :size="24" :src="'images/folib/medium.svg'" />
                              <span class="mb-0 text-dark">{{
                                scanReport.medium
                              }}</span>
                            </div>
                          </a-tooltip>

                          <a-tooltip>
                            <template slot="title">{{ $t('Store.LowRisk') }}</template>
                            <div class="">
                              <a-avatar :size="24" :src="'images/folib/low.svg'" />
                              <span class="mb-0 text-dark">{{
                                scanReport.low
                              }}</span>
                            </div>
                          </a-tooltip>
                        </template>
                        <template v-else>
                          <a-tooltip>
                            <template slot="title">{{ $t('Store.Health') }}</template>
                            <a-avatar :size="24" :src="'images/folib/healthy.svg'" />
                          </a-tooltip>
                        </template>
                      </a-space>
                    </span>
                    <span v-if="scanReport.fail">
                      <a-tag color="#f50">
                        {{ $t('Store.ScanFailure') }}
                      </a-tag>
                    </span>
                  </a-col>
                </a-row>
              </a-col>
              <a-col :span="8" class="text-right">
                <a-dropdown v-if="$store.state.user.token && currentTreeNode.url" class="mr-30" placement="bottomCenter">
                  <span style="font-size: 16px; cursor: pointer">
                    {{ $t('Store.More') }}
                    <a-icon type="more" class="text-muted" style="font-size: 16px" />
                  </span>
                  <template #overlay>
                    <a-menu slot="overlay" @click="handleMenuClick">
                      <a-menu-item key="1" v-if="currentFileDetial">
                        <a-icon type="eye" />
                        {{
                          currentFileDetial.listTree
                          ? $t('Store.Package')
                          : viewCodes
                            ? $t('Store.Document')
                            : folibRepository.layout === "Docker"
                              ? $t('Store.Details')
                              : ""
                        }}{{ $t('Store.Preview') }}
                      </a-menu-item>
                      <a-menu-item key="2" v-if="copyEnabled">
                        <a-icon type="copy" />{{ $t('Store.Copy') }}
                      </a-menu-item>
                      <a-menu-item key="3" v-if="moveEnabled">
                        <a-icon type="swap" />{{ $t('Store.Move') }}
                      </a-menu-item>
                      <a-menu-item key="4" v-if="deleteEnabled">
                        <a-popconfirm :title="$t('Store.SuerDelete')" placement="topLeft" okType="danger" :ok-text="$t('Store.Confirm')" :cancel-text="$t('Store.Cancel')"
                          @confirm="deletePackageHandle">
                          <a-icon type="delete" />{{ $t('Store.Delete') }}
                        </a-popconfirm>
                      </a-menu-item>
                      <a-menu-item key="5" v-if="dispatchEnabled">
                        <a-icon type="retweet" />{{ $t('Store.Distribute') }}
                      </a-menu-item>

                      <a-menu-item key="6"
                        v-if="folibRepository.layout !== 'Docker' && currentTreeNode && currentTreeNode.type === 'file' && currentFileDetial && currentFileDetial.artifact">
                        <a-icon type="download" />{{ $t('Store.DownLoad') }}
                      </a-menu-item>
                    </a-menu>
                  </template>
                </a-dropdown>
              </a-col>
            </a-row>
            <a-row type="flex" align="middle" v-if="folibRepository.layout === 'Docker'">
              <a-col :span="16" class="font-semibold m-0">
                <a-row type="flex" align="middle">
                  <a-col :span="8" :xs="24" :xl="16">
                    <a-avatar :size="24" shape="square" :src="'images/folib/docker-s.svg'" />
                    {{ currentTreeNode.name }}
                  </a-col>
                  <a-col :span="8" :xs="24" :xl="8">
                    <span class="ml-auto" v-if="scanReport.show" @click="openDetial">
                      <a-space :size="1" class="avatar-chips">
                        <template v-if="scanReport.vulnerabilitesCount > 0">
                          <a-tooltip>
                            <template slot="title">{{ $t('Store.Seriousness') }}</template>
                            <div class="">
                              <a-avatar :size="24" :src="'images/folib/critical.svg'" />
                              <span class="mb-0 text-dark">{{
                                scanReport.critical
                              }}</span>
                            </div>
                          </a-tooltip>

                          <a-tooltip>
                            <template slot="title">{{ $t('Store.HighRisk') }}</template>
                            <div class="">
                              <a-avatar :size="24" :src="'images/folib/high.svg'" />
                              <span class="mb-0 text-dark">{{
                                scanReport.high
                              }}</span>
                            </div>
                          </a-tooltip>

                          <a-tooltip>
                            <template slot="title">{{ $t('Store.MediumRisk') }}</template>
                            <div class="">
                              <a-avatar :size="24" :src="'images/folib/medium.svg'" />
                              <span class="mb-0 text-dark">{{
                                scanReport.medium
                              }}</span>
                            </div>
                          </a-tooltip>

                          <a-tooltip>
                            <template slot="title">{{ $t('Store.LowRisk') }}</template>
                            <div class="">
                              <a-avatar :size="24" :src="'images/folib/low.svg'" />
                              <span class="mb-0 text-dark">{{
                                scanReport.low
                              }}</span>
                            </div>
                          </a-tooltip>
                        </template>
                        <template v-else>
                          <a-tooltip>
                            <template slot="title">{{ $t('Store.Health') }}</template>
                            <a-avatar :size="24" :src="'images/folib/healthy.svg'" />
                          </a-tooltip>
                        </template>
                      </a-space>
                    </span>
                    <span v-if="scanReport.fail">
                      <a-tag color="#f50">
                        {{ $t('Store.ScanFailure') }}
                      </a-tag>
                    </span>
                  </a-col>
                </a-row>
              </a-col>


              <a-col :span="8" class="text-right">
                <a-dropdown v-if="$store.state.user.token && currentTreeNode.url" class="mr-45">
                  <span style="font-size: 16px; cursor: pointer">
                    {{ $t('Store.More') }}
                    <a-icon type="more" class="text-muted" style="font-size: 16px" />
                  </span>
                  <template #overlay>


                    <a-menu slot="overlay" @click="handleMenuClick">
                      <a-menu-item key="1" v-if="currentFileDetial">
                        <a-icon type="eye" />
                        {{
                          currentFileDetial.listTree
                          ? $t('Store.Package')
                              : viewCodes
                            ? $t('Store.Document')
                            : folibRepository.layout === "Docker"
                              ? $t('Store.Details')
                              : ""
                        }}{{ $t('Store.Preview') }}
                      </a-menu-item>
                      <a-menu-item key="2" v-if="copyEnabled">
                        <a-icon type="copy" />
                        {{ $t('Store.Copy') }}
                      </a-menu-item>
                      <a-menu-item key="3" v-if="moveEnabled">
                        <a-icon type="swap" />
                        {{ $t('Store.Move') }}
                      </a-menu-item>
                      <a-menu-item key="4" v-if="deleteEnabled">
                        <a-popconfirm :title="$t('Store.SuerDelete')" placement="topLeft" okType="danger" :ok-text="$t('Store.Confirm')" :cancel-text="$t('Store.Cancel')"
                          @confirm="deletePackageHandle">
                          <a-icon type="delete" />
                          {{ $t('Store.Delete') }}
                        </a-popconfirm>
                      </a-menu-item>
                      <a-menu-item key="5" v-if="dispatchEnabled">
                        <a-icon type="retweet" />
                        {{ $t('Store.Distribute') }}
                      </a-menu-item>

                      <a-menu-item key="6"
                        v-if="folibRepository.layout !== 'Docker' && currentTreeNode && currentTreeNode.type === 'file' && currentFileDetial && currentFileDetial.artifact">
                        <a-icon type="download" />
                        {{ $t('Store.DownLoad') }}
                      </a-menu-item>
                    </a-menu>
                  </template>
                </a-dropdown>
              </a-col>


            </a-row>
          </template>

          <a v-if="currentTreeNode.url && folibRepository.layout !== 'Docker'" class="text-dark" :href="currentTreeNode.url.search('http://localhost:38080/') !== -1
              ? currentTreeNode.url.replace(
                'http://localhost:38080/',
                baseUrl
              )
              : currentTreeNode.url
            " target="_blank">{{
    currentTreeNode.url.search("http://localhost:38080/") !== -1
    ? currentTreeNode.url.replace(
      "http://localhost:38080/",
      baseUrl
    )
    : currentTreeNode.url
  }}</a>

          <hr class="gradient-line" />
          <BaseData ref="BaseData" :currentTreeNode="currentTreeNode" :repositoryType="repositoryType"
            :currentFileDetial="currentFileDetial" :successMsg="successMsg" :folibRepository="folibRepository"
            @metadataEditHandler="metadataEditHandler" @metadataHandler="metadataHandler" @setCurrentFileDetial="setCurrentFileDetial"/>
        </a-card>
      </a-col>
    </a-row>
    <a-row v-if="isSearch === true" type="flex" :gutter="24">
      <!-- Platform Settings Column -->
      <Search ref="search" :columns="columns" :folibRepository="this.folibRepository" />
    </a-row>
    <use-doc :usedVisible="usedVisible" :repositoryType="repositoryType" :folibRepository="folibRepository"
      :ivyCode="ivyCode" :baseUrl="baseUrl" :dockerCode="dockerCode" @close="closeUsedVisibleDialog" />
    <!-- 预览 -->
    <a-drawer placement="right" width="45%" :title="currentTreeNode.name" :visible="viewCodeVisible"
      @close="closeViewCodeDialog">
      <div class="mx-auto m-50">
        <div class="mb-50">
          <a-card :bordered="false" class="header-solid">
            <a-directory-tree v-if="currentFileDetial && currentFileDetial.listTree"
              :replaceFields="{ title: 'name', children: 'children' }" :tree-data="currentFileDetial.listTree" />
          </a-card>
          <prism-editor class="my-editor height-300" v-if="currentFileDetial &&
            viewCodes &&
            folibRepository.layout !== 'Docker'
            " v-model="viewCodes" :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>

          <a-tabs v-if="currentFileDetial &&
            currentManifest &&
            folibRepository.layout === 'Docker'
            " class="tabs-sliding" default-active-key="1">
            <a-tab-pane key="1" tab="Layers">
              <a-timeline>
                <a-timeline-item color="primary" v-for="(key, index) in currentManifest.config" :key="index">
                  {{ index }}
                  <p>
                    {{ currentManifest.config[index] }}
                  </p>
                </a-timeline-item>
              </a-timeline>
            </a-tab-pane>
            <a-tab-pane key="2" :tab="$t('Store.ProductionHistory')">
              <a-timeline>
                <a-timeline-item color="primary" v-for="(key, index) in currentManifest.history" :key="index">
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
    <add-metadata v-if="showMetadataHandler" :showMetadataHandler="showMetadataHandler" :quillOptions="quillOptions"
      :handlerMetadataType="handlerMetadataType" :propMetadataForm="metadataForm" :metadataConfigList="metadataConfigList"
      :currentTreeNode="currentTreeNode" :metadataTypes="metadataTypes" :successMsg="successMsg"
      @metadataHandlerCancel="metadataHandlerCancel" @metadataReflesh="metadataReflesh" />

    <!-- 复制 -->
    <a-modal v-model="showOperationFormModal" :footer="null" :forceRender="true" :centered="true" :title="operationTitle"
      on-ok="showCopyFormModal = false">
      <a-form :form="operationForm" ref="operationForm" layout="vertical" @submit.prevent="handleOperationSubmit">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-item class="tags-field mb-10" :label="$t('Store.TargetWarehouse')" :colon="false" ref="targetRepositories"
              prop="targetRepositories">
              <gb-ant-select-two-cascader allowClear :placeholder="$t('Store.SelectTargetWarehouse')" v-decorator="[
                'targetRepositories',
                {
                  initialValue: [],
                  rules: [
                    {
                      required: true,
                      message: $t('Store.SelectTargetWarehouse'),
                      type: 'array',
                    },
                  ],
                },
              ]" :selectOptionsConfig="{
  key: 'key',
  value: 'key',
  text: 'name',
  children: 'children',
}" dropdownClassName="customer-multiple-cascader" :treeData="repositories" />
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
            <a-form-item class="tags-field mb-10" v-if="!custom" :label="$t('Store.TargetDirectory')" prop="path" :colon="false">
              <a-input v-decorator="[
                'path',
                {
                  rules: [{ required: true, message: $t('Store.TargetDirectory') }],
                },
              ]" :disabled="true" :placeholder="$t('Store.TargetDirectory')">
              </a-input>
            </a-form-item>
            <a-form-item class="tags-field mb-10" v-if="custom" :label="$t('Store.TargetDirectory')" prop="path" :colon="false">
              <a-input v-decorator="[
                'path',
                {
                  rules: [{ required: true, message: $t('Store.InputTargetDirectory') }],
                },
              ]" :disabled="false" :placeholder="$t('Store.InputTargetDirectory')">
              </a-input>
            </a-form-item>
          </a-col>
          <a-col :span="24" class="text-center">
            <a-button key="submit" class="px-30" size="small" type="primary" htmlType="submit">{{ $t('Store.Submit') }}</a-button>
            <a-button key="back" @click="operationFormModalClose()" class="px-30 ml-10" size="small">{{ $t('Store.Cancel') }}</a-button>
          </a-col>
        </a-row>
      </a-form>
    </a-modal>
    <!--    rpm 上传表单 start-->
    <a-modal v-model="showRpmUploadFormModal" :footer="null" :forceRender="true" :centered="true" :title="$t('Store.Upload')"
      on-ok="showRpmUploadFormModal = false">
      <a-form :form="rpmUploadForm" ref="rpmUploadForm" layout="horizontal" @submit.prevent="handleRpmUploadSubmit">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-item class="tags-field mb-10" :label="$t('Store.TargetWarehouse')" prop="repostoryId" :colon="false">
              <a-input v-decorator="[
                'repostoryId',
                {
                  rules: [{ required: true, message: $t('Store.InputWarehouse') }],
                },
              ]" :disabled="true" :placeholder="$t('Store.InputWarehouse')">
              </a-input>
            </a-form-item>
            <a-form-item :label="$t('Store.SelectFile')">
              <a-upload v-decorator="[
                'files',
                {
                  rules: [{ required: true, message: $t('Store.PleaseSelectFile') }],
                  valuePropName: 'fileList',
                  getValueFromEvent: normFile,
                },
              ]" name="files" :multiple="true" :beforeUpload="beforeUpload" list-type="text" accept=".rpm">
                <a-button>
                  <a-icon type="upload" />
                  {{ $t('Store.SelectFile') }} </a-button>
              </a-upload>
            </a-form-item>
          </a-col>
          <a-col :span="24" class="text-center">
            <a-button key="submit" class="px-30" size="small" type="primary" htmlType="submit">{{ $t('Store.Upload') }}</a-button>
            <a-button key="back" @click="uploadRpmFormModalClose()" class="px-30 ml-10" size="small">{{ $t('Store.Cancel') }}</a-button>
          </a-col>
        </a-row>
      </a-form>
    </a-modal>
    <!--   rpm 上传表单 end -->
    <!-- raw 、maven、npm 上传 -->
    <a-modal v-model="showUploadFormModal" :footer="null" :forceRender="true" :centered="true" :title="$t('Store.Upload')"
      on-ok="showUploadFormModal = false">
      <a-form :form="uploadForm" ref="uploadForm" layout="horizontal" @submit.prevent="handleUploadSubmit">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-item class="tags-field mb-10" :label="$t('Store.TargetWarehouse')" prop="repostoryId" :colon="false">
              <a-input v-decorator="[
                'repostoryId',
                {
                  rules: [{ required: true, message: $t('Store.InputWarehouse') }],
                },
              ]" :disabled="true" :placeholder="$t('Store.InputWarehouse')">
              </a-input>
            </a-form-item>
            <a-form-item :label="$t('Store.UploadMode')" v-if="folibRepository.layout === 'Maven 2'">
              <a-radio-group v-decorator="[
                'type',
                {
                  rules: [{ required: true, message: $t('Store.SelectUploadMode') }],
                },
              ]" @change="uploadTypeChange">
                <a-radio :value="1">
                  {{ $t('Store.Product') }}
                </a-radio>
                <a-radio :value="2">
                  {{ $t('Store.ZipUpload') }}
                </a-radio>
              </a-radio-group>
              <div>
                <span v-if="uploadType === 1">{{ $t('Store.ProductUpload') }}</span>
                <span v-if="uploadType === 2">{{ $t('Store.ZipFileUpload') }}{{ this.uploadMaxSize.size + this.uploadMaxSize.unit }}</span>
              </div>
            </a-form-item>
            <a-form-item :label="$t('Store.SelectFile')">
              <a-upload v-decorator="[
                'files',
                {
                  rules: [{ required: true, message: $t('Store.PleaseSelectFile') }],
                  valuePropName: 'fileList',
                  getValueFromEvent: normFile,
                },
              ]" name="files" :multiple="uploadType === 1 ? true : false" :beforeUpload="beforeUpload" list-type="text"
                :accept="uploadType === 1 ? (folibRepository.layout === 'Raw' ? '*' : folibRepository.layout === 'npm' ? '.tgz' : '.jar,.war,.pom') : ('.zip')">
                <a-button>
                  <a-icon type="upload" />
                  {{ $t('Store.SelectFile') }}</a-button>
              </a-upload>
            </a-form-item>
            <a-form-item class="tags-field mb-10" prop="targetPath" :colon="false"
              v-if="(folibRepository.layout !== 'Maven 2' && folibRepository.layout !== 'npm') || uploadType === 2">
              <template slot="label">
                {{ $t('Store.TargetDirectory') }}
                <a-popover placement="topLeft" v-if="uploadType === 2">
                  <template slot="content">
                    <p class="mb-0">{{ $t('Store.unzippedDirectory') }}</p>
                  </template>
                  <a class="ml-5">
                    <a-icon type="question-circle" theme="filled" /></a>
                </a-popover>
              </template>
              <a-input v-decorator="[
                'targetPath',
                {
                  rules: [
                    { required: false, message: $t('Store.InputTargetDirectory') }
                  ],
                },
              ]" :placeholder="$t('Store.InputTargetDirectory')">
              </a-input>
            </a-form-item>
          </a-col>
          <a-col :span="24" class="text-center">
            <a-button key="submit" class="px-30" size="small" type="primary" htmlType="submit">{{ $t('Store.Upload') }}</a-button>
            <a-button key="back" @click="uploadFormModalClose()" class="px-30 ml-10" size="small">{{ $t('Store.Cancel') }}</a-button>
          </a-col>
        </a-row>
      </a-form>
    </a-modal>
    <!--分发 -->
    <a-modal v-model="showOperationDispatchFormModal" width="50%" :footer="null" :forceRender="true" :centered="true"
      :title="operationTitle">
      <a-form :form="operationForm" ref="operationForm" layout="vertical" @submit.prevent="handleOperationSubmit">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-item class="tags-field mb-10" :label="$t('Store.NodeType')" :colon="true" v-if="this.folibRepository.layout === 'Raw'">
              <a-radio-group v-decorator="[
                'type',
                {
                  rules: [{ required: true, message: $t('Store.NodeTypeSelect') }],
                },
              ]"
              @change="typeChange">
                <a-radio :value="1">
                  <span>{{ $t('Store.InternalNode') }}</span>
                  <a-popover placement="topLeft">
                    <template slot="content">
                      <p class="mb-0">{{ instanceName + $t('Store.ProductWarehouseNode') }}</p>
                    </template>
                    <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                  </a-popover>
                </a-radio>
                <a-radio :value="2">
                  <span>{{ $t('Store.ExternalNode') }}</span>
                  <a-popover placement="topLeft">
                    <template slot="content">
                      <p class="mb-0">{{ $t('Store.OtherTypeNode') }}</p>
                    </template>
                    <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                  </a-popover>
                </a-radio>
              </a-radio-group>
            </a-form-item>
            <a-form-item class="tags-field mb-10" :label="$t('Store.TargetWarehouse')" :colon="false" ref="targetRepositories"
              prop="targetRepositories">
              <div class="selectdrop">
                <gb-ant-select-multiple-cascader allowClear style="width:100%;" :placeholder="$t('Store.SelectTargetWarehouse')" v-decorator="[
                  'targetRepositories',
                  {
                    initialValue: [],
                    rules: [
                      {
                        required: true,
                        message: $t('Store.SelectTargetWarehouse'),
                        type: 'array',
                      },
                    ],
                  },
                ]" :selectOptionsConfig="{
  key: 'key',
  value: 'key',
  text: 'key',
  children: 'children'
}" :allText="$t('Store.selectAll')" :noDataText="$t('Store.NoData')" dropdownClassName="customer-multiple-cascader"
                  :treeData="repositories" @handleCheckboxChange="handleCheckboxChange" v-if="artifactoryType === 1" />

                <gb-ant-select-two-cascader allowClear style="width:100%;" :placeholder="$t('Store.SelectTargetWarehouse')" v-decorator="[
                'targetRepositories',
                {
                  initialValue: [],
                  rules: [
                    {
                      required: true,
                      message: $t('Store.SelectTargetWarehouse'),
                      type: 'array',
                    },
                  ],
                },
              ]" :selectOptionsConfig="{
  key: 'key',
  value: 'key',
  text: 'key',
  children: 'children'
}" :allText="$t('Store.selectAll')" :noDataText="$t('Store.NoData')" dropdownClassName="customer-multiple-cascader"
                  :treeData="externalNodeRepositories"  v-if="artifactoryType === 2" />
              </div>
            </a-form-item>
            <a-form-item class="tags-field mb-10" v-if="!custom" :label="$t('Store.TargetDirectory')" prop="path" :colon="false">
              <a-input v-decorator="[
                'path',
                {
                  rules: [{ required: true, message: $t('Store.TargetDirectory') }],
                },
              ]" :disabled="true" :placeholder="$t('Store.InputTargetDirectory')">
              </a-input>
            </a-form-item>
            <a-form-item class="tags-field mb-10" v-if="custom" :label="$t('Store.TargetDirectory')" prop="path" :colon="false">
              <a-input v-decorator="[
                'path',
                {
                  rules: [{ required: true, message: $t('Store.InputTargetDirectory') }],
                },
              ]" :disabled="false" :placeholder="$t('Store.InputTargetDirectory')">
              </a-input>
            </a-form-item>
          </a-col>
          <a-col :span="24" class="text-center">
            <a-button key="submit" class="px-30" size="small" type="primary" htmlType="submit">{{ $t('Store.Submit') }}</a-button>
            <a-button key="back" @click="operationFormModalClose()" class="px-30 ml-10" size="small">{{ $t('Store.Cancel') }}</a-button>
          </a-col>
        </a-row>
      </a-form>
    </a-modal>

    <MavenUpload v-if="mavenUploadVisible" :modelVisible="mavenUploadVisible" :folibRepository="this.folibRepository"
      @mavenUploadClose="mavenUploadClose" />
  </div>
</template>

<script>
import store from 'store'
import uuidv4 from 'uuid/v4'
import {
  getLayoutType,
  getFileType,
  fileSizeConver,
  formateDate,
  artifactCheck
} from '@/utils/layoutUtil'
import {
  convertToBytes
} from '@/utils/util'
import {
  browse,
  getArtifact,
  previewArtifact,
  viewArtifactFile,
  fql,
  scannerRules,
  insertOrUpdateRules,
  getDockerArtifact,
  deleteArtifact,
  getPermissionStoragesAndRepositories,
  getStorageAndRepositoryPermission,
  getStoragesAndRepositories,
  getArtifactDispatchStoragesAndRepositories
} from '@/api/folib'
import {
  artifactCopy,
  artifactMove,
  artifactUpload,
  artifactUploadProgress,
  rpmArtifactUpload,
  artifactDispatch,
  artifactUploadZip
} from '@/api/artifact'
import { getMetadataConfiguration } from '@/api/settings'
import { hasRole, isAdmin, isAnonymous, isLogin } from '@/utils/permission'
import { getExternalNodeRepositories } from "@/api/externalNode"
import {
  getSingleDict
} from "@/api/advanced"
import SearchBox from '@/components/Tools/SearchBox'
import zhCN from 'ant-design-vue/es/locale/zh_CN'

import BaseData from './Data.vue'
import UseDoc from './UseDoc.vue'
import AddMetadata from './AddMetadata.vue'
import MavenUpload from '../MavenUpload/index.vue'
import Search from '../Search/index.vue'
import { PrismEditor } from 'vue-prism-editor'
import 'vue-prism-editor/dist/prismeditor.min.css' // import the styles somewhere
// import highlighting library (you can use any library you want just return html string)
import { highlight, languages } from 'prismjs/components/prism-core'
import 'prismjs/components/prism-clike'
import 'prismjs/components/prism-javascript'
import 'prismjs/themes/prism-tomorrow.css'
export default {
  inject: ['reload'],
  props: [
    'metadataTypes',
    'quillOptions',
    'successMsg',
    'searchType',
    'propScanReport',
    'formateDate'
  ],
  components: {
    PrismEditor,
    SearchBox,
    BaseData,
    UseDoc,
    AddMetadata,
    MavenUpload,
    Search
  },
  data () {
    return {
      baseUrl: '',
      folibRepository: {},
      repositoryType: null,
      rpmUploadForm: this.$form.createForm(this, { name: 'rpmUpload_form' }),
      uploadForm: this.$form.createForm(this, { name: 'upload_form' }),
      showUploadFormModal: false,
      showRpmUploadFormModal: false,
      uploadEnabled: false,
      copyEnabled: false,
      dispatchEnabled: false,
      moveEnabled: false,
      deleteEnabled: false,
      showFolibDownLoad: false,
      scan: {
        id: '',
        repository: '',
        storage: '',
        onScan: false,
        scanRule: null,
        layout: null
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
        value: undefined
      },
      metadataInput: true,
      metadataNumber: false,
      metadataEditor: false,
      prismEditor: false,
      codeParam: {
        type: '',
        code: null
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
        endDate: null
      },
      // 用户帮助
      usedVisible: false,
      ivyCode: null,
      dockerCode: { ubuntu: null, centos: null, windows: null, macos: null },
      // 预览
      operationForm: this.$form.createForm(this, { name: 'operation_form' }),
      viewCodeVisible: false,
      viewCodes: null,
      locale: zhCN,
      isSearch: false,
      searchData: [],
      searchDataCurrentSelect: {},
      searchViewCodeVisible: false,
      searchViewCodes: null,
      columns: [
        {
          title: this.$t('Store.OwnedWarehouse'),
          dataIndex: 'repositoryId',
          scopedSlots: { customRender: 'repositoryId' },
          width: 150
        },
        {
          title: this.$t('Store.ProductPath'),
          dataIndex: 'path',
          scopedSlots: { customRender: 'path' },
          width: 550
        },
        {
          title: this.$t('Store.CreationTime'),
          dataIndex: 'created',
          sorter: true,
          sortDirections: ['descend', 'ascend'],
          scopedSlots: { customRender: 'created' },
          width: 200
        },
        {
          title: this.$t('Store.LastUsedTime'),
          dataIndex: 'lastUsed',
          sorter: true,
          scopedSlots: { customRender: 'lastUsed' },
          width: 200
        },
        {
          title: this.$t('Store.DownloadTimes'),
          dataIndex: 'downloadCount',
          sorter: true,
          scopedSlots: { customRender: 'created' },
          width: 200
        },
        {
          title: this.$t('Store.ProductSize'),
          dataIndex: 'sizeInBytes',
          sorter: true,
          scopedSlots: { customRender: 'sizeInBytes' },
          width: 200
        }
      ],
      scanReport: {
        show: false,
        fail: false,
        report: [],
        vulnerabilitesCount: 0,
        critical: 0,
        high: 0,
        medium: 0,
        low: 0
      },
      operationTitle: '',
      showOperationFormModal: false,
      showOperationDispatchFormModal: false,
      repositories: [],
      custom: false,
      enablUploadedLayout: ['Raw', 'php', 'Maven 2', 'npm', 'rpm'],
      permissions: [],
      mavenUploadVisible: false,
      uploadType: 1,
      instanceName: '',
      externalNodeRepositories: [],
      artifactoryType: 1,
      uploadMaxSize: {
        size: 100,
        unit: 'MB',
      }
    }
  },
  created () {
    this.initData()
  },
  methods: {
    initData () {
      this.instanceName = sessionStorage.getItem("instanceName")
      this.createData()
      this.getBrowse()
      if (isLogin())
      {
        this.scannerRules()
        this.scanReport = Object.assign({}, this.propScanReport)
        this.queryStorageAndRepositoryPermission()
        this.getUploadMaxSize()
      }
    },
    scannerRules () {
      scannerRules(
        this.folibRepository.storageId + '-' + this.folibRepository.id
      ).then(res => {
        if (res.rel)
        {
          this.scan = res.data
        }
      })
    },
    handleCheckboxChange (selectedData) { },
    scannerChange () {
      this.scan.id =
        this.folibRepository.storageId + '-' + this.folibRepository.id
      this.scan.repository = this.folibRepository.id
      this.scan.storage = this.folibRepository.storageId
      this.scan.layout = this.folibRepository.layout
      insertOrUpdateRules(this.scan).then(res => {
        setTimeout(() => {
          this.$notification.success({
            message: this.scan.onScan ? this.$t('Store.ScanOn') : this.$t('Store.ScanOff')
          })
        }, 100)
      })
    },
    goBack () {
      if (isLogin())
      {
        this.$router.push({ name: 'storages' })
      } else
      {
        this.$router.push({ name: 'anonymousStorages' })
      }
    },
    getLayoutTypeHandle () {
      return getLayoutType(this.folibRepository)
    },
    getBrowse () {
      if (this.folibRepository.status.indexOf('Out of Service') !== -1)
      {
        this.$notification.warning({
          message: this.$t('Store.ServiceShutdown')
        })
        return false
      }
      if (!this.folibRepository.allowsDirectoryBrowsing)
      {
        this.$notification.warning({
          message: this.$t('Store.BrowseNotEnabled')
        })
        return false
      }
      browse(this.folibRepository.storageId, this.folibRepository.id, '')
        .then(res => {
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
        })
        .catch(err => { })
    },
    createData () {
      //上个页面通过缓存传参，目的防止页面刷新，路由数据消失
      const params = store.get('libView_repository')
      this.folibRepository = params.item
      this.baseUrl = params.baseUrl
      this.repositoryType = this.getLayoutTypeHandle()
    },
    copy (url) {
      var input = document.createElement('input') // 创建input对象
      input.value = url // 设置复制内容
      document.body.appendChild(input) // 添加临时实例
      input.select() // 选择实例内容
      document.execCommand('Copy') // 执行复制
      document.body.removeChild(input) // 删除临时实例
      // console.log(url)
      setTimeout(() => {
        this.$notification.success({
          message: this.$t('Store.CopySuccess')
        })
      }, 100)
    },
    handleRpmUpload () {
      this.rpmUploadForm.resetFields()
      this.$nextTick(() => {
        if (this.$refs.rpmUploadForm)
        {
          this.rpmUploadForm.setFieldsValue({
            repostoryId: this.folibRepository.id
          })
        }
      })
      this.showRpmUploadFormModal = true
    },
    uploadRpmFormModalClose () {
      this.rpmUploadForm.resetFields()
      this.showRpmUploadFormModal = false
    },
    beforeUpload (file, fileList) {
      return false
    },
    normFile (e) {
      if (Array.isArray(e))
      {
        return e
      }
      return e && e.fileList
    },
    handleUpload () {
      this.uploadForm.resetFields()
      this.$nextTick(() => {
        if (this.$refs.uploadForm)
        {
          let targetPath = ''
          if (this.folibRepository.layout === 'Raw') {
            if (this.currentTreeNode.type === 'dir') {
              targetPath = this.currentTreeNode.artifactPath
            } else if (this.currentTreeNode.type === 'file') {
              let length = this.currentTreeNode.artifactPath.length
              let nameLength = this.currentTreeNode.name.length
              targetPath = this.currentTreeNode.artifactPath.substring(0, length - nameLength)
              if (targetPath && targetPath.endsWith("/")) {
                targetPath = targetPath.substring(0, targetPath.length - 1)
              }
            }
          }
          this.uploadForm.setFieldsValue({
            repostoryId: this.folibRepository.id,
            type: 1,
            targetPath: targetPath,
          })
        }
      })
      this.uploadType = 1
      this.showUploadFormModal = true
    },
    message (type, message) {
      if (!message)
      {
        message = this.$t('Store.OperationSuccess')
      }
      this.$notification[type]({
        message: message,
        description: ''
      })
    },
    handleRpmUploadSubmit (e) {
      e.preventDefault()
      this.rpmUploadForm.validateFields((err, values) => {
        if (!err)
        {
          if (values.files.length > 10)
          {
            this.$notification['warning']({
              message: this.$t('Store.UploadCount'),
              description: ''
            })
            return false
          }
          let fileList = []
          for (let item of values.files)
          {
            let fileName = item.name.replace(':', '/')
            let result = artifactCheck(
              this.folibRepository,
              fileName,
              item.size
            )
            if (!result.check)
            {
              this.message('warning', result.msg)
              return false
            }
            item.name = fileName
            fileList.push(item)
          }
          fileList.forEach(item => {
            this.handlerRpmUploadFile(
              values.targetPath,
              item.name.replace(':', '/'),
              item.originFileObj
            )
          })
          this.successMsg(this.$t('Store.CheckProgress'))
          this.uploadRpmFormModalClose()
        }
      })
    },
    handlerRpmUploadFile (targetPath, fileName, file) {
      file = new File([file], fileName)
      let filePathMap = {}
      filePathMap[fileName] = targetPath
        ? targetPath + '/' + fileName
        : fileName
      let uuid = uuidv4()
      const formData = new FormData()
      formData.append('storageId', this.folibRepository.storageId)
      formData.append('repostoryId', this.folibRepository.id)
      formData.append('filePathMap', JSON.stringify(filePathMap))
      formData.append('files', file)
      rpmArtifactUpload(
        this.folibRepository.storageId,
        this.folibRepository.id,
        formData,
        uuid,
        fileName
      )
        .then(res => { })
        .catch(err => {
          let msg = err.response.data.error
            ? err.response.data.error
            : err.response.data
          console.log('rpm upload error：', msg)
          let errStatusArr = [200, 500, 403, 304, 401]
          if (!errStatusArr.includes(err.response.status))
          {
            this.$notification['error']({
              message: this.$t('Store.EncodingError') + err.response.status,
              description: ''
            })
          }
        })
        .finally(() => { })
    },
    getUploadMaxSize() {
      getSingleDict({ dictType: 'ui_upload_max_size' }).then(res => {
        if (res && res.dictValue) {
          this.uploadMaxSize = JSON.parse(res.dictValue)
        }
      })
    },
    convertToBytes (size, unit) {
      return convertToBytes(size, unit)
    },
    handleUploadSubmit (e) {
      e.preventDefault()
      this.uploadForm.validateFields((err, values) => {
        if (!err)
        {
          if (this.uploadType === 2)
          {
            if (values.files.length > 1)
            {
              this.$notification['warning']({
                message: this.$t('Store.UploadZipPackage'),
                description: ''
              })
              return false
            }
            const file = values.files[0]
            const sizeLimit = file.size > this.convertToBytes(this.uploadMaxSize.size, this.uploadMaxSize.unit)
            if (sizeLimit)
            {
              this.$notification.warning({
                message: this.$t('Store.fileSize') + this.uploadMaxSize.size + this.uploadMaxSize.unit
              })
              return false
            }
            const fileFamart = file.name.split('.')[
              file.name.split('.').length - 1
            ]
            if (fileFamart !== 'zip')
            {
              this.$notification.warning({
                message: this.$t('Store.ZIPFormat')
              })
              return false
            }
            if (typeof values.targetPath === 'undefined')
            {
              values.targetPath = ''
            } else
            {
              values.targetPath = values.targetPath
                .trim()
                .replace(/^\/+|\/+$/g, '')
            }
            this.handlerUploadZipFile(
              values.targetPath,
              file.name,
              file.originFileObj
            )
          } else
          {
            if (values.files.length > 10)
            {
              this.$notification['warning']({
                message: this.$t('Store.UploadCount'),
                description: ''
              })
              return false
            }
            if (values.targetPath && values.targetPath.startsWith('/'))
            {
              this.$notification['warning']({
                message: this.$t('Store.DirectoryFormat'),
                description: ''
              })
              return false
            }
            let fileList = []
            for (let item of values.files)
            {
              let fileName = item.name.replace(':', '/')
              let result = artifactCheck(
                this.folibRepository,
                fileName,
                item.size
              )
              if (!result.check)
              {
                this.message('warning', result.msg)
                return false
              }
              item.name = fileName
              fileList.push(item)
            }
            fileList.forEach(item => {
              this.handlerUploadFile(
                values.targetPath,
                item.name,
                item.originFileObj
              )
            })
          }
          this.successMsg(this.$t('Store.CheckProgress'))
          this.uploadFormModalClose()
        }
      })
    },
    handlerUploadFile (targetPath, fileName, file) {
      file = new File([file], fileName)
      let filePathMap = {}
      filePathMap[fileName] = targetPath
        ? targetPath + '/' + fileName
        : fileName
      let uuid = uuidv4()
      const formData = new FormData()
      formData.append('storageId', this.folibRepository.storageId)
      formData.append('repostoryId', this.folibRepository.id)
      formData.append('filePathMap', JSON.stringify(filePathMap))
      formData.append('files', file)
      artifactUploadProgress(formData, uuid, fileName)
        .then(res => { })
        .catch(err => {
          let msg = err.response.data.error
            ? err.response.data.error
            : err.response.data
          console.log('upload error：', msg)
          let errStatusArr = [200, 500, 403, 304, 401]
          if (!errStatusArr.includes(err.response.status))
          {
            this.$notification['error']({
              message: this.$t('Store.EncodingError') + err.response.status,
              description: ''
            })
          }
        })
        .finally(() => { })
    },
    handlerUploadZipFile (path, fileName, file) {
      file = new File([file], fileName)
      let uuid = 'zip_' + uuidv4()
      const formData = new FormData()
      formData.append('storageId', this.folibRepository.storageId)
      formData.append('repositoryId', this.folibRepository.id)
      formData.append('path', path)
      formData.append('file', file)
      artifactUploadZip(formData, uuid, fileName)
        .then(res => { })
        .catch(err => {
          let msg = err.response.data.error
            ? err.response.data.error
            : err.response.data
          console.log('upload error：', msg)
          let errStatusArr = [200, 500, 403, 304, 401]
          if (!errStatusArr.includes(err.response.status))
          {
            this.$notification['error']({
              message: this.$t('Store.EncodingError') + err.response.status,
              description: ''
            })
          }
        })
        .finally(() => { })
    },
    uploadFormModalClose () {
      this.showUploadFormModal = false
    },
    UsedHelperVisible () {
      if (this.repositoryType === 'ivy')
      {
        this.ivyCode =
          '<ivysettings>\n' +
          '   <settings defaultResolver="' +
          this.folibRepository.id +
          '" defaultConflictManager="all" />\n' +
          '   <resolvers>\n' +
          '        <ibiblio name="releases" root="' +
          this.baseUrl +
          'storages/' +
          this.folibRepository.storageId +
          '/' +
          this.folibRepository.id +
          '" m2compatible="true" usepoms="true"/>\n' +
          '   </resolvers>\n' +
          '</ivysettings>'
      } else if (this.repositoryType === 'docker')
      {
        this.dockerCode.ubuntu =
          'sudo mkdir -p /etc/docker\n' +
          "sudo tee /etc/docker/daemon.json <<-'EOF'\n" +
          '{\n' +
          '"insecure-registries": ["' +
          this.baseUrl.replace('http://', '').replace('/', '') +
          '"]\n' +
          '}\n' +
          'EOF\n' +
          'sudo systemctl daemon-reload\n' +
          'sudo systemctl restart docker'
        this.dockerCode.centos =
          'sudo mkdir -p /etc/docker\n' +
          "sudo tee /etc/docker/daemon.json <<-'EOF'\n" +
          '{\n' +
          '"insecure-registries": ["' +
          this.baseUrl.replace('http://', '').replace('/', '') +
          '"]\n' +
          '}\n' +
          'EOF\n' +
          'sudo systemctl daemon-reload\n' +
          'sudo systemctl restart docker'
        this.dockerCode.windows =
          '{\n' +
          '  "insecure-registries": ["' +
          this.baseUrl.replace('http://', '').replace('/', '') +
          '"]\n' +
          '}'
        this.dockerCode.macos = this.dockerCode.windows
      }
      this.usedVisible = true
    },
    scannerChange () {
      this.scan.id =
        this.folibRepository.storageId + '-' + this.folibRepository.id
      this.scan.repository = this.folibRepository.id
      this.scan.storage = this.folibRepository.storageId
      this.scan.layout = this.folibRepository.layout
      insertOrUpdateRules(this.scan).then(res => {
        setTimeout(() => {
          this.$notification.success({
            message: this.scan.onScan ? '开启扫描' : '关闭扫描'
          })
        }, 100)
      })
    },
    onLoadData (treeNode) {
      this.currentFileDetial = null
      if (this.folibRepository.layout === 'Docker')
      {
        return new Promise(resolve => {
          if (treeNode.dataRef.children)
          {
            resolve()
            return
          }
          getDockerArtifact(
            this.folibRepository.storageId,
            this.folibRepository.id,
            treeNode.dataRef.artifactPath
          ).then(res => {
            if (res.directories.length > 0)
            {
              const d = res.directories
              d.forEach((item, index, d) => {
                item.type = 'dir'
              })
              treeNode.dataRef.children = d
            } else if (res.files.length > 0)
            {
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

      return new Promise(resolve => {
        if (treeNode.dataRef.children)
        {
          resolve()
          return
        }
        browse(
          this.folibRepository.storageId,
          this.folibRepository.id,
          treeNode.dataRef.artifactPath
        ).then(res => {
          if (!treeNode.dataRef.children)
          {
            treeNode.dataRef.children = []
          }
          if (res.directories.length > 0)
          {
            const d = res.directories
            d.forEach((item, index, d) => {
              item.type = 'dir'
            })
            treeNode.dataRef.children = d
          }
          if (res.files.length > 0)
          {
            const a = res.files
            a.forEach((item, index, a) => {
              item.isLeaf = true
              item.type = 'file'
            })
            treeNode.dataRef.children = treeNode.dataRef.children.concat(a)
          }

          this.treeData = [...this.treeData]
          resolve()
        })
      })
    },
    treeSelect (key, e) {
      this.currentTreeNode = e.node.dataRef
      this.scanReport = {
        show: false,
        fail: false,
        report: [],
        vulnerabilitesCount: 0,
        critical: 0,
        high: 0,
        medium: 0,
        low: 0
      }
      if (this.currentTreeNode.type === 'file')
      {
        getArtifact(
          this.repositoryType,
          this.currentTreeNode.storageId,
          this.currentTreeNode.repositoryId,
          this.currentTreeNode.artifactPath
        ).then(res => {
          this.currentFileDetial = res
          if (this.currentFileDetial.snippets)
          {
            this.changeCodeTye(this.currentFileDetial.snippets[0])
          }
          if (isLogin() && this.currentFileDetial.artifact)
          {
            if (this.currentFileDetial.artifact.safeLevel === 'scanComplete')
            {
              this.scanReport.show = true
              this.scanReport.vulnerabilitesCount = this.currentFileDetial.artifact.vulnerabilitiesCount
              this.scanReport.critical = this.currentFileDetial.artifact.criticalVulnerabilitiesCount
              this.scanReport.high = this.currentFileDetial.artifact.highVulnerabilitiesCount
              this.scanReport.medium = this.currentFileDetial.artifact.mediumVulnerabilitiesCount
              this.scanReport.low = this.currentFileDetial.artifact.lowVulnerabilitiesCount
              this.scanReport.report = JSON.parse(
                this.currentFileDetial.artifact.report
              )
            } else if (this.currentFileDetial.artifact.safeLevel === 'scanFail') {
              this.scanReport.fail = true
            }
          }
          this.currentManifest = res.manifestConfig
          this.handlerRespMetadata(res)
        })
      } else if (this.currentTreeNode.type === 'dir')
      {
        this.currentFileDetial = null
      }

    },
    handleMenuClick (active) {
      this.operationForm.resetFields()
      this.$nextTick(() => {
        if (this.$refs.operationForm)
        {
          this.operationForm.setFieldsValue({
            path: this.currentTreeNode.artifactPath,
            type: 1,
          })
        }
      })
      if (active.key === '1')
      {
        this.viewCodeHandle()
      } else if (active.key === '2' || active.key === '3')
      {
        //复制 或 移动
        this.showOperationFormModal = true
        this.queryPermissionStoragesAndRepositories(
          this.folibRepository.type,
          this.folibRepository.layout,
          this.folibRepository.id,
          this.folibRepository.policy
        )
        this.operationTitle =
          active.key === '2'
            ? this.$t('Store.Copy')  + this.currentTreeNode.artifactPath
            : this.$t('Store.Move')  + this.currentTreeNode.artifactPath
        this.customTitle =
          active.key === '2' ? this.$t('Store.CopyCustomDirectory') : this.$t('Store.MoveCustomDirectory')
      } else if (active.key === '4')
      {
        //删除
      } else if (active.key === '5')
      {
        this.showOperationDispatchFormModal = true
        this.getArtifactDispatchStoragesAndRepositories(
          this.folibRepository.type,
          this.folibRepository.layout,
          this.folibRepository.id,
          this.folibRepository.policy
        )
        this.getExternalNodeRepositories()
        this.operationTitle = this.$t('Store.Distribute')
        this.customTitle = this.$t('Store.DistributeCustomDirectory')
        // 下载
      } else if (active.key === '6')
      {
        let url = this.currentTreeNode.url
        if (url)
        {
          window.open(url)
        }

      }
    },
    getArtifactoryRepositoryType(key) {
      let artifactoryRepositoryType = ''
      this.externalNodeRepositories.forEach(node => {
        let arr = node.children.filter(i => i.key === key)
        if(arr && arr.length > 0){
          artifactoryRepositoryType = arr[0].artifactoryRepositoryType
        }
      })
      return artifactoryRepositoryType
    },
    handleOperationSubmit (e) {
      e.preventDefault()
      this.operationForm.validateFields((err, values) => {
        if (!err)
        {
          let targetRepositoyList = []
          let targetDispatchRepositoryList = []
          values.targetRepositories.forEach(item => {
            let split = item.split(',')
            let arrayLength = split.length
            if (this.operationTitle.indexOf(this.$t('Store.Distribute')) !== -1)
            {
              let json = {}
              if (this.artifactoryType === 1) {
                let dispatchClusterEnName = split[0]
                let dispatchTargetStorageId = split[1]
                let dispatchTargetReopsitoryId = ''
                if (arrayLength === 3)
                {
                  dispatchTargetReopsitoryId = split[2]
                }
                json = {
                  dispatchClusterEnName: dispatchClusterEnName,
                  targetStorageId: dispatchTargetStorageId,
                  targetRepositoryId: dispatchTargetReopsitoryId
                }
                json.artifactoryRepositoryType = 'inner'
              } else {
                let dispatchClusterEnName = split[0]
                let dispatchTargetReopsitoryId = split[1]
                json = {
                  dispatchClusterEnName: dispatchClusterEnName,
                  targetRepositoryId: dispatchTargetReopsitoryId
                }
                json.artifactoryRepositoryType = this.getArtifactoryRepositoryType(item)
              }
              targetDispatchRepositoryList.push(json)
            } else
            {
              targetRepositoyList.push({
                targetStorageId: split[0],
                targetRepositoryId: split[1]
              })
            }
          })
          let data = {
            path: values.path,
            srcStorageId: this.folibRepository.storageId,
            srcRepositoryId: this.folibRepository.id,
            targetRepositoyList: targetRepositoyList
          }
          let dispatchData = {
            path: values.path,
            srcStorageId: this.folibRepository.storageId,
            srcRepositoryId: this.folibRepository.id,
            targetDispatchRepositoryList: targetDispatchRepositoryList,
            type: this.folibRepository.type,
            layout: this.folibRepository.layout,
            policy: this.folibRepository.policy
          }
          if (this.operationTitle.indexOf(this.$t('Store.Copy')) !== -1)
          {
            artifactCopy(data)
              .then(res => {
                this.successMsg(this.$t('Store.Copying'))
                this.operationFormModalClose()
                this.reload()
              })
              .catch(err => {
                this.$notification['error']({
                  message: err.response.data.error,
                  description: ''
                })
              })
              .finally(() => { })
          } else if (this.operationTitle.indexOf(this.$t('Store.Move')) !== -1)
          {
            artifactMove(data)
              .then(res => {
                this.successMsg(this.$t('Store.Moving'))
                this.operationFormModalClose()
                this.reload()
              })
              .catch(err => {
                this.$notification['error']({
                  message: err.response.data.error,
                  description: ''
                })
              })
              .finally(() => { })
          } else if (this.operationTitle.indexOf(this.$t('Store.Distribute')) !== -1)
          {
            artifactDispatch(dispatchData)
              .then(res => {
                this.successMsg(this.$t('Store.Distributing'))
                this.operationFormModalClose()
                this.reload()
              })
              .catch(err => {
                this.$notification['error']({
                  message: err.response.data.error,
                  description: ''
                })
              })
              .finally(() => { })
          }
        }
      })
    },
    operationFormModalClose () {
      this.showOperationFormModal = false
      this.showOperationDispatchFormModal = false
    },
    getArtifactDispatchStoragesAndRepositories (
      type,
      layout,
      excludeRepositoryId,
      policy
    ) {
      getArtifactDispatchStoragesAndRepositories({
        type: type,
        layout: layout,
        excludeRepositoryId: excludeRepositoryId,
        policy: policy
      }).then(res => {
        this.repositories = []
        res.forEach(item => {
          if (item.children && item.children.length > 0)
          {
            this.repositories.push(item)
          }
        })
        this.repositories = [this.repositories]
      })
    },
    queryPermissionStoragesAndRepositories (
      type,
      layout,
      excludeRepositoryId,
      policy
    ) {
      getPermissionStoragesAndRepositories({
        type: type,
        layout: layout,
        excludeRepositoryId: excludeRepositoryId,
        policy: policy
      }).then(res => {
        this.repositories = []
        res.forEach(item => {
          if (item.children && item.children.length > 0)
          {
            this.repositories.push(item)
          }
        })
      })
    },
    getMetadataConfiguration () {
      getMetadataConfiguration()
        .then(res => {
          this.metadataConfigList = res
        })
        .finally(() => { })
    },
    metadataHandler (type, metadata) {
      this.metadataFormReset()
      if (metadata)
      {
        this.metadataForm = metadata
      }
      this.handlerMetadataType = type
      this.showMetadataHandler = true
      this.getMetadataConfiguration()
    },
    metadataFormReset () {
      if (this.$refs.metadataForm)
      {
        this.$refs.metadataForm.resetFields()
      }
      this.metadataForm = {
        key: undefined,
        customKey: undefined,
        custom: false,
        type: undefined,
        viewShow: true,
        value: undefined
      }
      this.metadataInput = true
      this.metadataEditor = false
      this.metadataNumber = false
      this.prismEditor = false
    },
    dispatchPackageHandle () {
      console.log('分发处理 todo')
    },
    deletePackageHandle () {
      deleteArtifact(
        this.currentTreeNode.storageId,
        this.currentTreeNode.repositoryId,
        this.currentTreeNode.artifactPath
      )
        .then(res => {
          setTimeout(() => {
            this.$notification.success({
              message: this.$t('Store.DeletionSuccessful')
            })
            this.reload()
          }, 100)
        })
        .catch(err => {
          let errStatusArr = [403, 401]
          if (errStatusArr.includes(err.response.status)) {
            return false
          }
          let msg = err.response.data.message
            ? err.response.data.message
            : err.response.data.error
              ? err.response.data.error
              : err.response.data
          if (!msg || msg.length === 0 || typeof msg === 'object')
          {
            msg = this.$t('Store.DeletionFailed')
          }
          this.$notification.error({
            message: msg,
            description: ''
          })
        })
        .finally(() => { })
    },
    handlerRespMetadata (res) {
      let metadataList = []
      if (
        res.artifact &&
        res.artifact.metadata &&
        res.artifact.metadata.length > 0
      )
      {
        let metadataJson = JSON.parse(res.artifact.metadata)
        for (let key in metadataJson)
        {
          let flag = this.metadataConfigList.some(
            metadataConfig =>
              !metadataConfig.viewShow && metadataConfig.key === key
          )
          if (flag)
          {
            metadataJson[key].viewShow = false
          }
          let metadata = Object.assign({}, metadataJson[key])
          metadata.key = key
          metadataList.push(metadata)
        }
      }
      this.metadataList = metadataList
      this.$forceUpdate()
    },
    metadataEditorDrawerShow (metadata) {
      this.metadataEditorDrawerTitle = metadata.key
      this.metadataEditorDrawerValue = metadata.value
      this.metadataEditorDrawerVisible = true
    },
    metadataEditHandler (metadata) {
      let key = metadata.key
      let data = {
        key: undefined,
        customKey: undefined,
        custom: false,
        type: metadata.type,
        viewShow: metadata.viewShow === 1,
        value: metadata.value
      }
      let flag = this.metadataConfigList.some(item => item.key === key)
      if (!flag)
      {
        data.custom = true
        data.customKey = key
      } else
      {
        data.key = key
        data.custom = false
      }
      this.metadataHandler(2, data)
      this.metadataTypeChange(data.type)
    },
    metadataTypeChange (value) {
      let editorList = ['TEXT', 'MD']
      let prismEditorList = ['JSON']
      let numberList = ['NUMERICAL']
      if (editorList.indexOf(value) !== -1)
      {
        this.metadataEditor = true
        this.metadataInput = false
        this.metadataNumber = false
        this.prismEditor = false
      } else if (prismEditorList.indexOf(value) !== -1)
      {
        this.prismEditor = true
        this.metadataInput = false
        this.metadataNumber = false
        this.metadataEditor = false
      } else if (numberList.indexOf(value) !== -1)
      {
        if (this.handlerMetadataType === 1)
        {
          this.metadataForm.value = undefined
        }
        this.metadataNumber = true
        this.metadataInput = false
        this.prismEditor = false
        this.metadataEditor = false
      } else
      {
        this.metadataInput = true
        this.metadataEditor = false
        this.metadataNumber = false
        this.prismEditor = false
      }
    },
    metadataPrismEditorDrawerShow (metadata) {
      this.metadataPrismEditorDrawerTitle = metadata.key
      this.metadataPrismEditorDrawerValue = metadata.value
      this.metadataPrismEditorDrawerVisible = true
    },
    changeCodeTye (item) {
      if (item)
      {
        this.codeParam = {
          type: item.name === 'Maven 2' ? 'maven' : item.name.toLowerCase(),
          code: item.code
        }
      }
    },
    getFileType (name) {
      if (name)
      {
        return getFileType(name)
      }
    },
    closeUsedVisibleDialog () {
      this.usedVisible = false
    },
    viewCodeHandle () {
      if (this.folibRepository.layout !== 'Docker')
      {
        if (this.currentFileDetial && !this.currentFileDetial.listTree)
        {
          if (this.currentFileDetial.artifact) {
            previewArtifact(this.currentTreeNode.storageId, this.currentTreeNode.repositoryId,this.currentTreeNode.artifactPath).then(res => {
              if (res && res.length > 0) {
                this.currentFileDetial.listTree = res
                this.$forceUpdate()
              } else {
                let len = this.currentFileDetial.artifact.sizeInBytes
                if (len && len > 1048576) {
                  this.viewCodes = this.$t('Store.CannotPreview')
                } else{
                  this.viewArtifactFile()
                }
              }
            })
          } else {
            this.viewArtifactFile()
          }
      } else
      {
        // this.viewCodes=this.currentManifest.config
      }
      this.viewCodeVisible = true
    }
  },
  viewArtifactFile () {
    viewArtifactFile(this.currentTreeNode.url).then(res => {
      if ('string' === typeof res && res.startsWith('PK'))
      {
        this.viewCodes = undefined
      } else if ('object' === typeof res)
      {
        if (res.data)
        {
          if ('string' === typeof res.data)
          {
            if (res.data.startsWith('PK')) {
              this.viewCodes = this.$t('Store.CannotPreview')
            } else {
              this.viewCodes = res.data
            }
          } else
          {
            this.viewCodes = JSON.stringify(res.data)
          }
        } else
        {
          this.viewCodes = JSON.stringify(res)
        }
      } else
      {
        this.viewCodes = res
      }
    })
  },
    closeViewCodeDialog () {
      this.viewCodeVisible = false
      this.viewCodes = null
    },
    metadataHandlerCancel () {
      this.metadataFormReset()
      this.showMetadataHandler = false
    },
    metadataReflesh () {
      this.metadataFormReset()
      this.$refs.BaseData.getMetadata()
      this.showMetadataHandler = false
    },
    search (value, searchType, type) {
      this.isSearch = true
      this.$nextTick(() => {
        this.$refs.search.search(value, searchType, type)
      })
    },
    onPageSizeChange () {
      this.search(this.artifactQuery.artifactName, 1)
    },
    handleTableChange (pagination, filters, sorter) {
      this.artifactQuery.sortField = null
      this.artifactQuery.sortOrder = null
      if (pagination)
      {
        this.artifactQuery.page = pagination.current
      }
      if (sorter)
      {
        this.artifactQuery.sortField = sorter.field
        if (sorter.order)
        {
          this.artifactQuery.sortOrder = 'asc'
          if (sorter.order.indexOf('desc') !== -1)
          {
            this.artifactQuery.sortOrder = 'desc'
          }
        }
      }
      this.search(this.artifactQuery.artifactName)
    },
    dateChange (value, dateString) {
      if (dateString)
      {
        this.artifactQuery.beginDate = dateString[0]
        this.artifactQuery.endDate = dateString[1]
        if (
          this.artifactQuery.beginDate === '' &&
          this.artifactQuery.endDate === ''
        )
        {
          this.dateConfirm()
        }
      }
    },
    dateConfirm () {
      this.search(this.artifactQuery.artifactName, 1)
    },
    openDetial () {
      this.$emit('openDetial', this.scanReport)
    },
    highlighterHandle (code) {
      return highlight(code, languages.js) //returns html
    },
    fileSizeConver (size) {
      if (size)
      {
        return fileSizeConver(size)
      }
    },
    queryStorageAndRepositoryPermission () {
      this.permissions = []
      getStorageAndRepositoryPermission(
        this.folibRepository.storageId,
        this.folibRepository.id
      ).then(res => {
        this.permissions = res
        this.uploadEnabled =
          this.folibRepository.status.indexOf('Out of Service') === -1 &&
          this.enablUploadedLayout.includes(this.folibRepository.layout) &&
          this.folibRepository.type === 'hosted' &&
          (hasRole('ARTIFACTS_MANAGER') ||
            this.permissions.includes('ARTIFACTS_DEPLOY'))
        this.copyEnabled =
          this.folibRepository.type === 'hosted' &&
          (hasRole('ARTIFACTS_MANAGER') ||
            this.permissions.includes('ARTIFACTS_COPY'))
        this.dispatchEnabled =
          this.folibRepository.type === 'hosted' && isAdmin()
        this.moveEnabled =
          this.folibRepository.type === 'hosted' &&
          (hasRole('ARTIFACTS_MANAGER') ||
            this.permissions.includes('ARTIFACTS_MOVE'))
        this.deleteEnabled =
          this.folibRepository.type !== 'group' &&
          (hasRole('ARTIFACTS_MANAGER') ||
            this.permissions.includes('ARTIFACTS_DELETE'))

      })

    },
    getRepositoryUrl () {
      let repositoryUrl = ''
      if (this.baseUrl)
      {
        repositoryUrl =
          this.baseUrl +
          'storages/' +
          this.folibRepository.storageId +
          '/' +
          this.folibRepository.id
        let layout = this.folibRepository.layout.toLowerCase()
        if (layout === 'docker' || layout === 'conan') {
          let baseUrlArr = this.baseUrl.split('://')
          repositoryUrl =
            baseUrlArr[1] +
            this.folibRepository.storageId +
            '/' +
            this.folibRepository.id
        }
      }
      return repositoryUrl
    },
    handleMavenUpload () {
      this.mavenUploadVisible = true
    },
    mavenUploadClose () {
      this.mavenUploadVisible = false
    },
    uploadTypeChange (element) {
      if (element.target.value === 1)
      {
        this.uploadType = 1
      } else if (element.target.value === 2)
      {
        this.uploadType = 2
      }
    },
    setCurrentFileDetial(currentFileDetial) {
      if (currentFileDetial) {
        this.currentFileDetial = currentFileDetial
        this.currentManifest = currentFileDetial.manifestConfig
        this.$forceUpdate()
      }
    },
    getExternalNodeRepositories() {
      getExternalNodeRepositories().then(res => {
        if (res) {
          res.forEach(node => {
            let json = {key: node.key, artifactoryRepositoryType: '', children: [], }
            node.repositories.forEach(repo => {
              json.children.push({key: repo.key, artifactoryRepositoryType: repo.artifactoryRepositoryType, children: null})
            })
            this.externalNodeRepositories.push(json)
          })
        }
      }).finally(() => {
      })
    },
    typeChange(event) {
      this.artifactoryType = event.target.value
      this.operationForm.setFieldsValue({
        targetRepositories: [],
      })
    }
  }
}
</script>
<style lang="scss" scoped>
.repo-info::v-deep {
  .selectdrop .gb-ant-select-multiple-cascader .cascader-content-wrap .cascader-content-container .cascader-content-list {
    min-width: 280px;
  }
  .copy-p {
    display: inline-block;
  }
  .repo-address .ant-descriptions-item-label {
    margin-left: 0px !important;
  }
}
</style>
