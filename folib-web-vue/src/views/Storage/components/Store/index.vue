<template>
  <div class="repo-info">
    <a-affix :offset-top="50" class="repository-affix">
      <a-row>
        <a-col :span="24" :md="24" class="mb-24">
            <!-- 进度球-->
<!--            <CircleProgress :progress="totalUploadProgress" :closeGlobe="isClose" :waveClassName="waveClassName" :containerClassName="containerClassName"/>-->
          <!-- User Profile Card -->
          <a-card :bordered="false" class="card-profile-head" :bodyStyle="{ padding: 0 }" :targetOffset="0"
            :affix="false">
            <template #title>
              <a-row type="flex" align="middle">
                <a-col :span="24" :md="12" class="col-info">
                  <a v-if="!isChecked">
                    <a-icon type="backward" :style="{
                      fontSize: '32px',
                      marginRight: '5px',
                      opacity: '0.8',
                      color: '#BFBFBFFF',
                    }" @click="goBack()" />
                  </a>
                  <a style="justify-content: center;align-items: center;display: flex;">
                    <a-avatar @click="createData" :size="84" shape="square"
                      style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );   "
                                >
                        <img :src="'images/folib/' + getLayoutTypeHandle() + '.svg'" style="width: 150%;margin-left: -13.5px;" alt=""></img>
                    </a-avatar>
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
                  <a-col :span="16" :md="7" class="col-info">

                          <div style="width: 85%">
                              <a-progress
                                  v-if="isClose"
                                  :stroke-color="{from: '#108ee9',to: '#87d068',}"
                                  :percent="totalUploadProgress"
                                  :status="progressStatus"
                              />
                          </div>
                  </a-col>
                <a-col :span="8" :md="5" style="
                    display: flex;
                    align-items: center;
                    justify-content: flex-end;
                  ">
                  <a v-if="folibRepository.layout === 'Docker' && folibRepository.type === 'hosted'">
                    <small style="padding-right: 20px" @click="handleDockerUploud">
                      {{ $t('Store.Upload') }}
                      <a-icon type="cloud-upload" />
                    </small>
                  </a>
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
                  <a v-if="uploadEnabled && folibRepository.layout === 'debian'"><small style="padding-right: 20px"
                      @click="handleDebianUpload">
                    {{ $t('Store.Upload') }}
                      <a-icon type="cloud-upload" />
                    </small>
                  </a>
                  <a v-if="uploadEnabled && folibRepository.layout !== 'rpm' &&  folibRepository.layout !== 'GitLfs' && folibRepository.layout !== 'GitLfs' && folibRepository.subLayout !== 'ohpm' && folibRepository.subLayout !== 'go' && folibRepository.layout !== 'debian'"><small style="padding-right: 20px" @click="handleUpload">
                      {{ $t('Store.BatchUpload') }}
                      <a-icon type="cloud-upload" />
                    </small>
                  </a>
                  <a v-if="uploadEnabled && folibRepository.layout === 'debian'"><small style="padding-right: 20px" @click="handleDebianBatchUpload">
                      {{ $t('Store.BatchUpload') }}
                      <a-icon type="cloud-upload" />
                    </small>
                  </a>
                  <a v-if="folibRepository.layout !== 'Raw'">
                    <small style="padding-right: 20px" @click="UsedHelperVisible">
                      {{ $t('Store.UseHelp') }}
                      <a-icon type="question-circle" theme="filled" />
                    </small>
                  </a>
                  <div v-if="(isAdmin() || (storageAdmin && storageAdmin === $store.state.user.name)) && folibRepository.type !== 'group'">
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
      <a-col style="margin-top:-20px;" v-if="!isChecked" :span="24" :md="10" class="mb-24">
        <a-card :bordered="false" style="max-height: 1024px; min-height: 554px; overflow-y: auto" class="header-solid"
          :bodyStyle="{ paddingTop: 0, paddingBottom: 0 }">
          <template #title>
            <h6 class="font-semibold m-0">{{ isTrashView ? $t('Store.TrashCan') : $t('Store.PacketList') }} <a
                v-show="!isTrashView" class="ml-10" @click="reloadTreeData">
                <a-icon type="reload" /></a></h6>
          </template>
          <!-- <a-button slot="extra" type="link"  size="large" style="color: black;" @click="isTrashView=!isTrashView"> -->
          <a-tooltip slot="extra" @click="isTrashView = !isTrashView" class="view-switch">
            <template slot="title">
              {{ isTrashView ? $t('Store.PacketListView') : $t('Store.TrashCanView') }}
            </template>
            <a-icon v-if="isTrashView" type="file-zip" />
            <a-icon v-if="!isTrashView" type="delete" />
          </a-tooltip>
          <!-- </a-button> -->
          <a-directory-tree v-if="!isTrashView" :replaceFields="{
            key: 'artifactPath',
            title: 'name',
            children: 'children',
          }" :tree-data="treeData" :load-data="onLoadData" @select="treeSelect" @rightClick="onRightClick">
          </a-directory-tree>
          <a-directory-tree v-if="isTrashView" :replaceFields="{
            key: 'artifactPath',
            title: 'name',
            children: 'children',
          }" :tree-data="trashData" :load-data="onLoadData" @select="treeSelect" @rightClick="onRightClick">

          </a-directory-tree>
        </a-card>
      </a-col>
      <a-col style="margin-top:-20px;" :span="24" :md="!isChecked ? 14 : 24" class="mb-24">
        <a-card :bordered="false" class="header-solid h-full card-profile-information"
          :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }" :headStyle="{ paddingRight: 0 }">
          <template #title v-if="isChecked ? !newDetailPage : true ">
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
                <a-dropdown v-if="$store.state.user.token && currentTreeNode.url" class="mr-30"
                  placement="bottomCenter">
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
                      <a-menu-item key="2" v-if="copyEnabled&&!isTrashView">
                        <a-icon type="copy" />{{ $t('Store.Copy') }}
                      </a-menu-item>
                      <a-menu-item key="3" v-if="moveEnabled&&!isTrashView">
                        <a-icon type="swap" />{{ $t('Store.Move') }}
                      </a-menu-item>
                      <a-menu-item key="4" v-if="deleteEnabled&&!isTrashView">
                        <a-popconfirm :title="$t('Store.SuerDelete')" placement="topLeft" okType="danger"
                          :ok-text="$t('Store.Confirm')" :cancel-text="$t('Store.Cancel')"
                          @confirm="deletePackageHandle">
                          <a-icon type="delete" />{{ $t('Store.Delete') }}
                        </a-popconfirm>
                      </a-menu-item>
                      <a-menu-item key="5" v-if="dispatchEnabled&&!isTrashView">
                        <a-icon type="retweet" />{{ $t('Store.Distribute') }}
                      </a-menu-item>

                      <a-menu-item key="6"
                        v-if="folibRepository.layout !== 'Docker' && currentTreeNode && currentTreeNode.type === 'file' && currentFileDetial && currentFileDetial.artifact&&!isTrashView">
                        <a-icon type="download" />{{ $t('Store.DownLoad') }}
                      </a-menu-item>

                      <a-menu-item key="7"
                        v-if="(folibRepository.layout === 'Raw' && currentTreeNode && currentTreeNode.type === 'dir'&&!isTrashView)">
                        <a-icon type="download" />{{ $t('Store.DownLoad') }}
                      </a-menu-item>
                      <a-menu-item key="8" v-if="isTrashView && currentTreeNode">
                        <a-icon type="undo" />{{ $t('Store.Restore') }}
                      </a-menu-item>
                      <a-modal :title="$t('Store.Prompts')" :visible="downLoadVisible" :okText="$t('Store.Confirm')"
                        :cancelText="$t('Store.Cancel')" centered @ok="handleDownLoadDir"
                        @cancel="handleDownLoadDirCancel">
                        <p>{{ currentTreeNode.artifactPath + $t('Store.DirSize') + rawPathSize + ", "+$t('Store.ConfirmDownload') }}</p>
                      </a-modal>
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
                      <a-menu-item key="2" v-if="copyEnabled&&!isTrashView">
                        <a-icon type="copy" />
                        {{ $t('Store.Copy') }}
                      </a-menu-item>
                      <a-menu-item key="3" v-if="moveEnabled&&!isTrashView">
                        <a-icon type="swap" />
                        {{ $t('Store.Move') }}
                      </a-menu-item>
                      <a-menu-item key="4" v-if="deleteEnabled&&!isTrashView">
                        <a-popconfirm :title="$t('Store.SuerDelete')" placement="topLeft" okType="danger"
                          :ok-text="$t('Store.Confirm')" :cancel-text="$t('Store.Cancel')"
                          @confirm="deletePackageHandle">
                          <a-icon type="delete" />
                          {{ $t('Store.Delete') }}
                        </a-popconfirm>
                      </a-menu-item>
                      <a-menu-item key="5" v-if="dispatchEnabled&&!isTrashView">
                        <a-icon type="retweet" />
                        {{ $t('Store.Distribute') }}
                      </a-menu-item>

                      <a-menu-item key="6"
                        v-if="folibRepository.layout !== 'Docker' && currentTreeNode && currentTreeNode.type === 'file' && currentFileDetial && currentFileDetial.artifact&&!isTrashView">
                        <a-icon type="download" />
                        {{ $t('Store.DownLoad') }}
                      </a-menu-item>

                      <a-menu-item key="7"
                        v-if="folibRepository.layout === 'Docker' && currentTreeNode && currentTreeNode.type === 'file' && currentFileDetial && currentFileDetial.artifact&&!isTrashView">
                        <a-icon type="download" />
                        {{ $t('Store.DownLoad') }}
                      </a-menu-item>
                      <a-menu-item key="8" v-if="isTrashView && currentTreeNode">
                        <a-icon type="undo" />{{ $t('Store.Restore') }}
                      </a-menu-item>
                    </a-menu>
                  </template>
                </a-dropdown>
              </a-col>
            </a-row>
          </template>

          <a v-if="currentTreeNode.url && folibRepository.layout !== 'Docker'" class="text-dark ellipsis-link"
            :href="getFormattedUrl(currentTreeNode.url)" target="_blank" :title="getFormattedUrl(currentTreeNode.url)">
            {{ getFormattedUrl(currentTreeNode.url) }}
          </a>
          <a v-if="currentTreeNode.url && folibRepository.layout !== 'Docker'" class="ml-10"><a-icon type="copy"
              @click="copy(getFormattedUrl(currentTreeNode.url))" /> </a>
          <hr class="gradient-line" />
          <BaseData 
              ref="BaseData"
              :key="pageKey"
              :isChecked="isChecked" 
              :currentTreeNode="currentTreeNode" 
              :repositoryType="repositoryType"
              :currentFileDetial="currentFileDetial" 
              :successMsg="successMsg" 
              :folibRepository="folibRepository" 
              @messageArchitectureChild="handleArchitectureMessage"
              @metadataEditHandler="metadataEditHandler" 
              @metadataHandler="metadataHandler" 
              @setCurrentFileDetial="setCurrentFileDetial"
          />
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
          " v-model="viewCodes" :highlight="highlighterHandle" :line-numbers="false"
            :readonly="true"></prism-editor>

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
      :handlerMetadataType="handlerMetadataType" :propMetadataForm="metadataForm"
      :metadataConfigList="metadataConfigList" :currentTreeNode="currentTreeNode" :metadataTypes="metadataTypes"
      :successMsg="successMsg" @metadataHandlerCancel="metadataHandlerCancel" @metadataReflesh="metadataReflesh" />

    <!-- 复制 -->
    <a-modal v-model="showOperationFormModal" :footer="null" :forceRender="true" :centered="true"
      :title="operationTitle" on-ok="showCopyFormModal = false">
      <a-form :form="operationForm" ref="operationForm" layout="vertical" @submit.prevent="handleOperationSubmit">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-item class="tags-field mb-10" :label="$t('Store.TargetWarehouse')" :colon="false"
              ref="targetRepositories" prop="targetRepositories">
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
            <a-form-item class="tags-field mb-10" v-if="!custom" :label="$t('Store.TargetDirectory')" prop="path" :colon="false" style="display:none;">
              <a-input v-decorator="[
                'path',
                {
                  rules: [{ required: true, message: $t('Store.TargetDirectory') }],
                },
              ]" :disabled="true" :placeholder="$t('Store.TargetDirectory')">
              </a-input>
            </a-form-item>
              <a-form-item class="tags-field mb-10" v-if="!custom" :label="$t('Store.TargetDirectory')" prop="targetPath" :colon="false" >
                  <a-input v-decorator="[
                'targetPath',
                {
                  rules: [{ required: true, message: $t('Store.TargetDirectory') }],
                },
              ]" :disabled="isTargetPatDisabled" :placeholder="$t('Store.TargetDirectory')" >
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
                  rules: [{ required: true, message: $t('Store.InputTargetWarehouse') }],
                },
              ]" :disabled="true" :placeholder="$t('Store.InputTargetWarehouse')">
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
              ]" name="files" :multiple="true" :beforeUpload="beforeUpload" @change="onFileChange" list-type="text" accept=".rpm">
                <a-button>
                  <a-icon type="upload" />
                  {{ $t('Store.SelectFile') }} </a-button>
              </a-upload>
            </a-form-item>
          </a-col>
          <a-col :span="24" class="text-center">
            <a-button key="submit" class="px-30" size="small" type="primary" :disabled="isUploading || !md5CalculationComplete" htmlType="submit">{{ $t('Store.Upload') }}</a-button>
            <a-button key="back" @click="uploadRpmFormModalClose()" class="px-30 ml-10" size="small">{{ $t('Store.Cancel') }}</a-button>
          </a-col>
        </a-row>
      </a-form>
    </a-modal>
    <!--   rpm 上传表单 end -->
    <!-- docker上传表单 -->
    <a-modal v-model="showDockerUploadFormModal" :footer="null" :forceRender="true" :centered="true"
               :title="$t('Store.Upload')"
               on-ok="showDockerUploadFormModal = false">
          <a-form :form="dockerUploadForm" ref="dockerUploadForm" layout="horizontal" @submit.prevent="handleDockerUploadSubmit">
              <a-row :gutter="[24]">
                  <a-col :span="24">
                      <a-form-item class="tags-field mb-10" :label="$t('Store.TargetWarehouse')" prop="repostoryId" :colon="false">
                          <a-input  :disabled="true" :placeholder="$t('Store.InputTargetWarehouse')" v-model="folibRepository.id">
                          </a-input>
                      </a-form-item>

                  </a-col>
                  <a-col :span="24">
                    <a-form-item :label="$t('Store.UploadMode')">
                      <a-radio-group v-decorator="[
                        'type',
                        {
                          rules: [{ required: true, message: $t('Store.SelectUploadMode') }],
                        },
                      ]">
                        <a-radio value="image">
                          {{ $t('Store.Image') }}
                        </a-radio>
                        <a-radio value="subsidiary">
                          {{ $t('Store.SubsidiaryFiles') }}
                        </a-radio>
                      </a-radio-group>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" >

                      <a-form-item :label="$t('Store.SelectFile')">
                          <a-upload v-decorator="[
                'files',
                {
                  rules: [{ required: true, message: $t('Store.PleaseSelectFile') }],
                  valuePropName: 'fileList',
                  getValueFromEvent: normFile,
                },
              ]" name="files" :multiple="false" :beforeUpload="beforeUpload" @change="onFileChange" list-type="text" accept=".gz,.tar,.zip,.giz">
                              <a-button>
                                  <a-icon type="upload"/>
                                  {{ $t('Store.SelectFile') }}
                              </a-button>
                          </a-upload>
                      </a-form-item>

                  </a-col>
                  <a-col :span="24">
                      <a-form-item class="tags-field mb-10 label-with-icon "  :label="$t('Store.ImageTag')" prop="imageTag"
                                   :colon="false">
                          <div>
                              <span> {{ $t('Store.ImageTagSpecification') }}</span>
                          </div>
                          <a-input
                                  v-decorator="[
                                      'imageTag',
                                      {
                                        rules: [
                                          {
                                            required: true,
                                            pattern: /^[a-zA-Z0-9_\-\./]+(?:\/[a-zA-Z0-9_\-\./]+)?:[a-zA-Z0-9_\-\./]+$/,
                                            message: $t('Store.InputImageTag'),
                                          },
                                        ],
                                      },
                                    ]"
                                  :disabled="false"
                                  :placeholder="$t('Store.InputImageTag')"
                          />
                      </a-form-item>
                  </a-col>


                  <a-col :span="24" class="text-center">
                      <a-button key="submit" class="px-30" size="small" type="primary" :disabled="isUploading || !md5CalculationComplete" htmlType="submit">
                          {{ $t('Store.Upload') }}
                      </a-button>
                      <a-button key="back" @click="uploadDockerFormModalClose()" class="px-30 ml-10" size="small">
                          {{ $t('Store.Cancel') }}
                      </a-button>
                  </a-col>
              </a-row>
          </a-form>
      </a-modal>

    <!-- raw 、maven、npm 上传 -->
    <a-modal v-model="showUploadFormModal" :footer="null" :forceRender="true" :centered="true"
      :title="$t('Store.Upload')" on-ok="showUploadFormModal = false">
      <a-form :form="uploadForm" ref="uploadForm" layout="horizontal" @submit.prevent="handleUploadSubmit">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-item class="tags-field mb-10" :label="$t('Store.TargetWarehouse')" prop="repostoryId"
              :colon="false">
              <a-input v-decorator="[
                'repostoryId',
                {
                  rules: [{ required: true, message: $t('Store.InputTargetWarehouse') }],
                },
              ]" :disabled="true" :placeholder="$t('Store.InputTargetWarehouse')">
              </a-input>
            </a-form-item>
            <a-form-item :label="$t('Store.UploadMode')"
              v-if="folibRepository.layout === 'Maven 2' || folibRepository.layout === 'Raw'">
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
                <span v-if="uploadType === 2">{{ $t('Store.ZipFileUpload') }}{{ this.uploadMaxSize.size +
                  this.uploadMaxSize.unit }}</span>
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
              ]" name="files" :multiple="uploadType === 1 ? true : false" :beforeUpload="beforeUpload" list-type="text" @change="onFileChange"
                :accept="uploadType === 1 ? (folibRepository.layout === 'Raw' ? '*' : folibRepository.layout === 'npm' ? '.tgz' : folibRepository.layout === 'pub' ? '.gz' :'.jar,.war,.pom') : ('.zip')">
                <a-button>
                  <a-icon type="upload" />
                  {{ $t('Store.SelectFile') }}</a-button>
              </a-upload>
            </a-form-item>
            <a-form-item class="tags-field mb-10" prop="targetPath" :colon="false"
              v-if="(!targetDirectoryExcludeLayout.includes(folibRepository.layout)) || uploadType === 2">
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
            <a-button key="submit" class="px-30" size="small" type="primary" htmlType="submit" :disabled="isUploading || !md5CalculationComplete">{{ $t('Store.Upload') }}</a-button>
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
            <a-form-item class="tags-field mb-10" :label="$t('Store.NodeType')" :colon="true"
              v-if="this.enableUnionRepository.includes(this.folibRepository.layout)">
              <a-radio-group v-decorator="[
                'type',
                {
                  rules: [{ required: true, message: $t('Store.NodeTypeSelect') }],
                },
              ]" @change="typeChange">
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
            <a-form-item class="tags-field mb-10" :label="$t('Store.TargetWarehouse')" :colon="false"
              ref="targetRepositories" prop="targetRepositories">
              <div class="selectdrop">
                <a-tree-select v-decorator="[
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
                ]" style="width: 100%" treeCheckable :maxTagCount="6"
                  :dropdown-style="{ maxHeight: '400px', overflow: 'auto' }" :tree-data="repositories"
                  :placeholder="$t('Store.SelectTargetWarehouse')" allow-clear show-search
                  :replaceFields="{ children: 'children', title: 'key', key: 'key', value: 'key' }"
                  v-if="artifactoryType === 1">
                </a-tree-select>
                <gb-ant-select-two-cascader allowClear style="width:100%;"
                  :placeholder="$t('Store.SelectTargetWarehouse')" v-decorator="[
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
                  :treeData="externalNodeRepositories" v-if="artifactoryType === 2" />
              </div>
            </a-form-item>
            <a-form-item class="tags-field mb-10" v-if="!custom" :label="$t('Store.TargetDirectory')" prop="path"
              :colon="false">
              <a-input v-decorator="[
                'path',
                {
                  rules: [{ required: true, message: $t('Store.TargetDirectory') }],
                },
              ]" :disabled="true" :placeholder="$t('Store.InputTargetDirectory')">
              </a-input>
            </a-form-item>
            <a-form-item class="tags-field mb-10" v-if="custom" :label="$t('Store.TargetDirectory')" prop="path"
              :colon="false">
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
            <a-button key="submit" class="px-30" size="small" type="primary" htmlType="submit">{{ $t('Store.Submit')
              }}</a-button>
            <a-button key="back" @click="operationFormModalClose()" class="px-30 ml-10" size="small">{{
              $t('Store.Cancel')
            }}</a-button>
          </a-col>
        </a-row>
      </a-form>
    </a-modal>

    <MavenUpload v-if="mavenUploadVisible" :modelVisible="mavenUploadVisible" :folibRepository="this.folibRepository"
      @mavenUploadClose="mavenUploadClose" />
    <DebianUpload  ref="debianmodal" :folibRepository="folibRepository"/>
    <DebianBatchUpload  ref="debianBatchModal" :folibRepository="folibRepository"/>
    <div v-if="showContextMenu" :style="contextMenuStyle" class="context-menu">
      <a-menu @click="handleRightClick">
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
        <a-menu-item key="2" v-if="copyEnabled && !isTrashView">
          <a-icon type="copy" />
          {{ $t('Store.Copy') }}
        </a-menu-item>
        <a-menu-item key="3" v-if="moveEnabled && !isTrashView">
          <a-icon type="swap" />
          {{ $t('Store.Move') }}
        </a-menu-item>
        <a-menu-item key="4" v-if="deleteEnabled && !isTrashView">
          <!-- <a-popconfirm :title="$t('Store.SuerDelete')" okType="danger"
              :ok-text="$t('Store.Confirm')" :cancel-text="$t('Store.Cancel')" @confirm.stop="deletePackageHandle"  :style="{ zIndex: 2000 }"> -->
          <a-icon type="delete" />
          {{ $t('Store.Delete') }}
          <!-- </a-popconfirm> -->
        </a-menu-item>
        <a-menu-item key="5" v-if="dispatchEnabled && !isTrashView">
          <a-icon type="retweet" />
          {{ $t('Store.Distribute') }}
        </a-menu-item>

        <a-menu-item key="6"
          v-if="folibRepository.layout !== 'Docker' && currentTreeNode && currentTreeNode.type === 'file' && currentFileDetial && currentFileDetial.artifact && !isTrashView">
          <a-icon type="download" />
          {{ $t('Store.DownLoad') }}
        </a-menu-item>
        <a-menu-item key="7"
          v-if="folibRepository.layout === 'Docker' && currentTreeNode && currentTreeNode.type === 'file' && currentFileDetial && currentFileDetial.artifact && !isTrashView">
          <a-icon type="download" />
          {{ $t('Store.DownLoad') }}
        </a-menu-item>
        <a-menu-item key="8" v-if="isTrashView && currentTreeNode">
          <a-icon type="undo" />{{ $t('Store.Restore') }}
        </a-menu-item>
      </a-menu>
    </div>
  </div>
</template>

<script>
import store from 'store'
import storage from "store";
import Cookies from "js-cookie";
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
  getArtifactReport,
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
  getArtifactDispatchStoragesAndRepositories,
} from '@/api/folib'
import {
  artifactCopy,
  artifactMove,
  artifactUpload,
  artifactUploadProgress,
  rpmArtifactUpload,
  artifactDispatch,
  artifactUploadZip, getRawPathSize
} from '@/api/artifact'
import { getMetadataConfiguration, restore, } from '@/api/settings'
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
import DebianUpload from '../Debian/DebianUpload.vue'
import DebianBatchUpload from '../Debian/DebianBatchUpload.vue'
import Search from '../Search/index.vue'
import { PrismEditor } from 'vue-prism-editor'
import 'vue-prism-editor/dist/prismeditor.min.css' // import the styles somewhere
// import highlighting library (you can use any library you want just return html string)
import { highlight, languages } from 'prismjs/components/prism-core'
import 'prismjs/components/prism-clike'
import 'prismjs/components/prism-javascript'
import 'prismjs/themes/prism-tomorrow.css'
import SparkMD5 from 'spark-md5';
import {ACCESS_TOKEN} from "@/store/mutation-types";
import CircleProgress from '@/components/Tools/CircleProgress.vue';
export default {
  inject: ['reload'],
  props: [
    'metadataTypes',
    'quillOptions',
    'successMsg',
    'searchType',
    'propScanReport',
    'formateDate',
    'isChecked'
  ],
  components: {
    PrismEditor,
    SearchBox,
    BaseData,
    UseDoc,
    AddMetadata,
    MavenUpload,
    Search,
    DebianUpload,
    DebianBatchUpload,
    CircleProgress
  },
  data() {
    return {
      downLoadVisible: false,
      downLoadLoading: true,
      rawPathSize: "",
      baseUrl: '',
      folibRepository: {},
      repositoryType: null,
      rpmUploadForm: this.$form.createForm(this, { name: 'rpmUpload_form' }),
      uploadForm: this.$form.createForm(this, { name: 'upload_form' }),
      dockerUploadForm: this.$form.createForm(this, { name: 'dockerUpload_form' }),
      restoreForm: this.$form.createForm(this, { name: 'restore_form' }),
      showUploadFormModal: false,
      showRpmUploadFormModal: false,
      showDockerUploadFormModal: false,
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
      trashData: [],
      currentFileDetial: null,
      currentTreeNode: {},
      detialVisible: false,
      targetArchitecture: null,
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
      //目标目录是否disabled
      isTargetPatDisabled: true,
      isTrashView: false,
      columns: [
        {
          i18nKey: 'Store.OwnedWarehouse',
          title: this.$t('Store.OwnedWarehouse'),
          dataIndex: 'repositoryId',
          scopedSlots: { customRender: 'repositoryId' },
          width: 150
        },
        {
          i18nKey: 'Store.ProductPath',
          title: this.$t('Store.ProductPath'),
          dataIndex: 'path',
          scopedSlots: { customRender: 'path' },
          width: 550
        },
        {
          i18nKey: 'Store.CreationTime',
          title: this.$t('Store.CreationTime'),
          dataIndex: 'created',
          sorter: true,
          sortDirections: ['descend', 'ascend'],
          scopedSlots: { customRender: 'created' },
          width: 200
        },
        {
          i18nKey: 'Store.LastUsedTime',
          title: this.$t('Store.LastUsedTime'),
          dataIndex: 'lastUsed',
          sorter: true,
          scopedSlots: { customRender: 'lastUsed' },
          width: 200
        },
        {
          i18nKey: 'Store.DownloadTimes',
          title: this.$t('Store.DownloadTimes'),
          dataIndex: 'downloadCount',
          sorter: true,
          scopedSlots: { customRender: 'created' },
          width: 200
        },
        {
          i18nKey: 'Store.ProductSize',
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
      enablUploadedLayout: ['Raw', 'php', 'Maven 2', 'npm', 'rpm', 'go','GitLfs', 'pub','debian'],
      targetDirectoryExcludeLayout: ['Maven 2', 'npm', 'pub'],
      storageAdmin: '',
      permissions: [],
      mavenUploadVisible: false,
      uploadType: 1,
      instanceName: '',
      externalNodeRepositories: [],
      artifactoryType: 1,
      uploadMaxSize: {
        size: 100,
        unit: 'MB',
      },
      enableUnionRepository: [
        "Raw",
        "Maven 2",
        "Docker"
      ],
      sliceUploadData: {
          file: null,
          chunkSize: 5 * 1024 * 1024, // 分片大小 5MB
          uploadProgress: 0, //进度
          currentChunk: 0, // 当前分片
          totalChunks: 0, // 分片总数
          isUploading: false, // 是否正在上传
          paused: false, // 是否暂停
          fileId: '', // 文件的唯一标识，用于断点续传
          fileMD5: '', // 文件的md5值
          progress: 0, // md5进度
      },
        selectedFiles: [], // 保存选中的多个文件
        chunkSize: 5 * 1024 * 1024, // 分片大小 5MB
        uploadProgresses: [], // 保存每个文件的上传进度
        currentChunks: [], // 保存每个文件的当前上传分片索引
        totalChunks: [], // 保存每个文件的总分片数
        isUploading: false,
        paused: false,
        fileIds: [], // 保存每个文件的唯一标识
        fileMD5s: [], // 保存每个文件的 MD5 值
        md5Progresses: [], // 保存每个文件 MD5 计算的进度
        maxConcurrency: 1, // 最大并发数
        activeUploads: 0, // 当前的并发上传数
        totalUploadProgress: 0, // 总的上传进度
        totalUploadSize: 0, // 总的上传大小
        uploadedSize: 0, // 当前已上传的大小
        md5CalculationComplete: false, // MD5 计算是否完成
        isClose: false,
        progressStatus:'active',

	    // 右击菜单
	    showContextMenu: false,
	    rightClickTop: '0px',
	    rightClickLeft: '0px',
	    packageSelectedKeys: [],
	    packageExpandedKeys: [],
	    packageLoadedKeys: [],
	    restoreTitle: null,
	    showRestoreForm: false,
      pageKey:0
    }
  },
  computed: {
    newDetailPage(){
      return this.$store.state.newDetailPage
    },
    contextMenuStyle() {
      return {
        position: 'fixed',
        top: this.rightClickTop,
        left: this.rightClickLeft,
      }
    },
    i18nColumns() {
      return this.columns.map(column => {
        if (column.i18nKey) {
          column.title = this.$t(column.i18nKey);
        }
        return column;
      })
    },
  },
  watch:{
    currentTreeNode(val){
      console.log(val,'watch currentTreeNode');
      this.pageKey ++
    } 
  },
  created () {
    this.initData()
  },

  mounted() {
    document.addEventListener('click', this.closeContextMenu);
  },
  beforeDestroy() {
    document.removeEventListener('click', this.closeContextMenu);
  },
  methods: {
    initData() {
      this.instanceName = sessionStorage.getItem("instanceName")
      this.createData()
      if(!this.isChecked){
        this.getBrowse()
      }
      if (isLogin())
      {
        this.scannerRules()
        this.scanReport = Object.assign({}, this.propScanReport)
        this.queryStorageAndRepositoryPermission()
        this.getUploadMaxSize()
      }
    },
    scannerRules() {
      scannerRules(
        this.folibRepository.storageId + '-' + this.folibRepository.id
      ).then(res => {
        if (res.rel) {
          this.scan = res.data
        }
      })
    },
    reloadTreeData() {
      this.reload();
    },
    handleCheckboxChange(selectedData) { },
    scannerChange() {
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
    goBack() {
      this.$router.push({ name: 'storagesHome' })
    },
    getLayoutTypeHandle() {
      return getLayoutType(this.folibRepository)
    },
    getBrowse() {
      if (this.folibRepository.status.indexOf('Out of Service') !== -1) {
        this.$notification.warning({
          message: this.$t('Store.ServiceShutdown')
        })
        return false
      }
      if (!this.folibRepository.allowsDirectoryBrowsing) {
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
          this.treeData = d.concat(f).filter(item => item.name !== '.trash')
          this.trashData = d.concat(f).filter(item => item.name === '.trash')
        })
        .catch(err => { })
    },
    createData() {
      //上个页面通过缓存传参，目的防止页面刷新，路由数据消失
      const params = store.get('libView_repository')
      this.folibRepository = params.item
      this.baseUrl = params.baseUrl
      this.repositoryType = this.getLayoutTypeHandle()
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
          message: this.$t('Store.CopySuccess')
        })
      }, 100)
    },
    handleDockerUploud() {
      this.dockerUploadForm.resetFields()
      this.$nextTick(() => {
        if (this.$refs.dockerUploadForm) {
          this.dockerUploadForm.setFieldsValue({
            repositoryId: this.folibRepository.id
          })
          this.dockerUploadForm.setFieldsValue({
            type: 'image'
          })
        }
      })
      this.showDockerUploadFormModal = true;
    },
    uploadDockerFormModalClose () {
          this.dockerUploadForm.resetFields()
          this.showDockerUploadFormModal = false
          this.isUploading=false;
          this.md5CalculationComplete=false;
    },
    handleRpmUpload() {
      this.rpmUploadForm.resetFields()
      this.$nextTick(() => {
        if (this.$refs.rpmUploadForm) {
          this.rpmUploadForm.setFieldsValue({
            repostoryId: this.folibRepository.id
          })
        }
      })
      this.showRpmUploadFormModal = true
    },
    uploadRpmFormModalClose() {
      this.rpmUploadForm.resetFields()
      this.showRpmUploadFormModal = false
      this.isUploading=false;
      this.md5CalculationComplete=false;
    },
    beforeUpload(file, fileList) {
      return false
    },
    normFile(e) {
      if (Array.isArray(e)) {
        return e
      }
      return e && e.fileList
    },
    handleUpload() {
      this.uploadForm.resetFields()
      this.$nextTick(() => {
        if (this.$refs.uploadForm) {
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
    message(type, message) {
      if (!message) {
        message = this.$t('Store.OperationSuccess')
      }
      this.$notification[type]({
        message: message,
        description: ''
      })
    },
    handleDockerUploadSubmit (e) {
          e.preventDefault()
          this.dockerUploadForm.validateFields((err, values) => {
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
                  // fileList.forEach(item => {
                  //     this.handlerDockerUploadFile(
                  //         values.type,
                  //         values.imageTag,
                  //         item.name.replace(':', '/'),
                  //         item.originFileObj
                  //     )
                  // })
                  console.info('docker:', values)
                  const imageTag = values.imageTag ? values.imageTag : fileList[0].originFileObj.name;
                  this.uploadFiles(values.targetPath,false,null,imageTag,values.type)
                  this.successMsg(this.$t('Store.CheckProgress'))
                  this.uploadDockerFormModalClose()
              }
          })
      },
    handleRpmUploadSubmit (e) {
      e.preventDefault()
      this.rpmUploadForm.validateFields((err, values) => {
        if (!err) {
          if (values.files.length > 10) {
            this.$notification['warning']({
              message: this.$t('Store.UploadCount'),
              description: ''
            })
            return false
          }
          let fileList = []
          for (let item of values.files) {
            let fileName = item.name.replace(':', '/')
            let result = artifactCheck(
              this.folibRepository,
              fileName,
              item.size
            )
            if (!result.check) {
              this.message('warning', result.msg)
              return false
            }
            item.name = fileName
            fileList.push(item)
          }
          // fileList.forEach(item => {
          //   this.handlerRpmUploadFile(
          //     values.targetPath,
          //     item.name.replace(':', '/'),
          //     item.originFileObj
          //   )
          // })
            this.uploadFiles(values.targetPath,false,null,null,null)
          this.successMsg(this.$t('Store.CheckProgress'))
          this.uploadRpmFormModalClose()
        }
      })
    },
    handlerDockerUploadFile (fileType, imageTag, fileName, file) {
          file = new File([file], fileName)
          let filePathMap ={};
          filePathMap[fileName] = imageTag ? imageTag : fileName;
          const formData = new FormData()
          formData.append('storageId', this.folibRepository.storageId)
          formData.append('repostoryId', this.folibRepository.id)
          formData.append('filePathMap', JSON.stringify(filePathMap))
          formData.append('imageTag', imageTag)
          formData.append('fileType', fileType)
          formData.append('files', file)
          let uuid = uuidv4()
          artifactUploadProgress(formData, uuid, fileName)
                .then(res => { })
                .catch(err => {
                    let msg = err.response.data.error
                        ? err.response.data.error
                        : err.response.data
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
          if (!errStatusArr.includes(err.response.status)) {
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
    convertToBytes(size, unit) {
      return convertToBytes(size, unit)
    },
    handleUploadSubmit(e) {
      e.preventDefault()
      this.uploadForm.validateFields((err, values) => {
        if (!err) {
          if (this.uploadType === 2) {
            if (values.files.length > 1) {
              this.$notification['warning']({
                message: this.$t('Store.UploadZipPackage'),
                description: ''
              })
              return false
            }
            const file = values.files[0]
            const sizeLimit = file.size > this.convertToBytes(this.uploadMaxSize.size, this.uploadMaxSize.unit)
            if (sizeLimit) {
              this.$notification.warning({
                message: this.$t('Store.fileSize') + this.uploadMaxSize.size + this.uploadMaxSize.unit
              })
              return false
            }
            const fileFamart = file.name.split('.')[
              file.name.split('.').length - 1
            ]
            if (fileFamart !== 'zip') {
              this.$notification.warning({
                message: this.$t('Store.ZIPFormat')
              })
              return false
            }
            if (typeof values.targetPath === 'undefined') {
              values.targetPath = ''
            } else {
              values.targetPath = values.targetPath
                .trim()
                .replace(/^\/+|\/+$/g, '')
            }
            // this.handlerUploadZipFile(
            //   values.targetPath,
            //   file.name,
            //   file.originFileObj
            // )
             //this.onFileChange(values.targetPath, file.name, values.files[0].originFileObj)
             this.uploadFiles(values.targetPath,true,null,null,null)
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
            if (values.targetPath && values.targetPath.startsWith('/')) {
              this.$notification['warning']({
                message: this.$t('Store.DirectoryFormat'),
                description: ''
              })
              return false
            }
            let fileList = []
            for (let item of values.files) {
              let fileName = item.name.replace(':', '/')
              let result = artifactCheck(
                this.folibRepository,
                fileName,
                item.size
              )
              if (!result.check) {
                this.message('warning', result.msg)
                return false
              }
              item.name = fileName
              fileList.push(item)
            }
            // fileList.forEach(item => {
            //   this.handlerUploadFile(
            //     values.targetPath,
            //     item.name,
            //     item.originFileObj
            //   )
            //    this.onFileChange(values.targetPath, item.name, item.originFileObj)
            // })
            this.uploadFiles(values.targetPath,false,null,null,null)
          }
          this.successMsg(this.$t('Store.CheckProgress'))
          this.uploadFormModalClose()
        }
      })
    },
    handlerUploadFile(targetPath, fileName, file) {
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
          if (!errStatusArr.includes(err.response.status)) {
            this.$notification['error']({
              message: this.$t('Store.EncodingError') + err.response.status,
              description: ''
            })
          }
        })
        .finally(() => { })
    },
    handlerUploadZipFile(path, fileName, file) {
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
          if (!errStatusArr.includes(err.response.status)) {
            this.$notification['error']({
              message: this.$t('Store.EncodingError') + err.response.status,
              description: ''
            })
          }
        })
        .finally(() => { })
    },
    uploadFormModalClose() {
      this.showUploadFormModal = false
        this.isUploading=false;
        this.md5CalculationComplete=false;
    },
    UsedHelperVisible() {
      if (this.repositoryType === 'ivy') {
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
      } else if (this.repositoryType === 'docker') {
        this.dockerCode.ubuntu =
          'sudo mkdir -p /etc/docker\n' +
          "sudo tee /etc/docker/daemon.json <<-'EOF'\n" +
          '{\n' +
          '"insecure-registries": ["' +
          this.baseUrl.replace('http://', '').replace('https://', '').replace('/', '') +
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
          this.baseUrl.replace('http://', '').replace('https://', '').replace('/', '') +
          '"]\n' +
          '}\n' +
          'EOF\n' +
          'sudo systemctl daemon-reload\n' +
          'sudo systemctl restart docker'
        this.dockerCode.windows =
          '{\n' +
          '  "insecure-registries": ["' +
          this.baseUrl.replace('http://', '').replace('https://', '').replace('/', '') +
          '"]\n' +
          '}'
        this.dockerCode.macos = this.dockerCode.windows
      }
      this.usedVisible = true
    },
    // scannerChange () {
    //   this.scan.id =
    //     this.folibRepository.storageId + '-' + this.folibRepository.id
    //   this.scan.repository = this.folibRepository.id
    //   this.scan.storage = this.folibRepository.storageId
    //   this.scan.layout = this.folibRepository.layout
    //   insertOrUpdateRules(this.scan).then(res => {
    //     setTimeout(() => {
    //       this.$notification.success({
    //         message: this.scan.onScan ? '开启扫描' : '关闭扫描'
    //       })
    //     }, 100)
    //   })
    // },
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
            treeNode.dataRef.children = []
            if (res.directories.length > 0) {
              const d = res.directories

              d.forEach((item, index, d) => {
                item.type = 'dir'
                treeNode.dataRef.children.push(item)
              })
            }
            if (res.files.length > 0) {
              const a = res.files
              a.forEach((item, index, a) => {
                item.isLeaf = true
                item.type = 'file'
                treeNode.dataRef.children.push(item)
              })
            }
            this.treeData = [...this.treeData]
            this.trashData = [...this.trashData]
            resolve()
          })
        })
      }

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
          if (!treeNode.dataRef.children) {
            treeNode.dataRef.children = []
          }
          if (res.directories.length > 0) {
            const d = res.directories
            d.forEach((item, index, d) => {
              item.type = 'dir'
            })
            treeNode.dataRef.children = d
          }
          if (res.files.length > 0) {
            const a = res.files
            a.forEach((item, index, a) => {
              item.isLeaf = true
              item.type = 'file'
            })
            treeNode.dataRef.children = treeNode.dataRef.children.concat(a)
          }

          this.treeData = [...this.treeData]
          this.trashData = [...this.trashData]
          resolve()
        })
      })
    },
    treeSelect(key, e) {
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
      if (this.currentTreeNode.type === 'file'){
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
          if (isLogin() && this.currentFileDetial.artifact) {
            if (this.currentFileDetial.artifact.safeLevel === "scanComplete") {
              this.scanReport.show = true
              this.scanReport.vulnerabilitesCount = this.currentFileDetial.artifact.vulnerabilitiesCount
              this.scanReport.critical = this.currentFileDetial.artifact.criticalVulnerabilitiesCount
              this.scanReport.high = this.currentFileDetial.artifact.highVulnerabilitiesCount
              this.scanReport.medium = this.currentFileDetial.artifact.mediumVulnerabilitiesCount
              this.scanReport.low = this.currentFileDetial.artifact.lowVulnerabilitiesCount
              getArtifactReport(this.repositoryType,
                this.currentTreeNode.storageId,
                this.currentTreeNode.repositoryId,
                this.currentTreeNode.artifactPath
              ).then(res => {
                if (res.artifact && res.artifact.safeLevel === "scanComplete") {
                  this.scanReport.report = JSON.parse(
                    res.artifact.report
                  )
                }
              })
            } else if (this.currentFileDetial.artifact.safeLevel === 'scanFail') {
              this.scanReport.fail = true
            }
          }
          this.currentManifest = res.manifestConfig
          this.handlerRespMetadata(res)
        })
      } else if (this.currentTreeNode.type === 'dir') {
        this.currentFileDetial = null
      }

    },
    handleMenuClickTree(active,currentTreeNode){
      this.currentTreeNode = currentTreeNode
      this.handleMenuClick(active)
    },
    handleMenuClick(active) {
      this.operationForm.resetFields()
      this.isTargetPatDisabled = this.folibRepository.layout !== 'Raw';
      this.$nextTick(() => {
        if (this.$refs.operationForm) {
          this.operationForm.setFieldsValue({
            path: this.currentTreeNode.artifactPath,
            targetPath: this.currentTreeNode.artifactPath,
            type: 1,
          })
        }
      })
      if (active.key === '1') {
        this.viewCodeHandle()
      } else if (active.key === '2' || active.key === '3') {
        //复制 或 移动
        this.showOperationFormModal = true
        this.queryPermissionStoragesAndRepositories(
          this.folibRepository.type,
          this.folibRepository.layout,
          this.folibRepository.storageId + ':' + this.folibRepository.id,
          this.folibRepository.policy
        )
        this.operationTitle =
          active.key === '2'
            ? this.$t('Store.Copy') + this.currentTreeNode.artifactPath
            : this.$t('Store.Move') + this.currentTreeNode.artifactPath
        this.customTitle =
          active.key === '2' ? this.$t('Store.CopyCustomDirectory') : this.$t('Store.MoveCustomDirectory')
      } else if (active.key === '4') {
        //删除
        console.log("删除")
      } else if (active.key === '5') {
        this.showOperationDispatchFormModal = true
        this.getArtifactDispatchStoragesAndRepositories(
          this.folibRepository.type,
          this.folibRepository.layout,
          this.folibRepository.policy
        )
        this.getExternalNodeRepositories({ type: this.folibRepository.layout })
        this.operationTitle = this.$t('Store.Distribute')
        this.customTitle = this.$t('Store.DistributeCustomDirectory')
        // 下载  
      } else if (active.key === '6') {
        let url = this.currentTreeNode.url
        if (url) {
          window.open(url)
        }

      } else if (active.key === '7') {
        if (this.currentTreeNode.type === 'dir') {
          this.downLoadVisible = true;
          this.folibRepository
          let storageId = this.folibRepository.storageId;
          let repositoryId = this.folibRepository.id;
          let path = this.currentTreeNode.artifactPath;
          getRawPathSize(storageId, repositoryId, path).then(res => {
            this.rawPathSize = res;

          })
          // let url = this.currentTreeNode.url
          // if (url) {
          //     url = url.replace("api/browse", "storages")
          //     window.open(url)
          // }
        } else if (this.currentTreeNode.type === 'file') {
          let uri = this.currentTreeNode.url;
          const str = this.currentFileDetial.imageName;

          // 使用正则表达式匹配第三个 '/' 后的部分
          const regex = /^([^\/]*\/){3}(.*)$/;
          const match = str.match(regex);

          const result = match ? match[2] : '';
          if (uri) {
            const url = new URL(uri);
            // 获取协议（http: 或 https:）
            const protocol = url.protocol;
            // 获取主机名（不包括路径和查询字符串）
            const hostname = url.hostname;
            // 获取端口号，如果没有指定则默认为 80（http）或 443（https）
            const port = url.port ? `:${url.port}` : '';
            const params = this.targetArchitecture === null ? '' : '?platform=' + this.targetArchitecture;
            const baseUrl = `${protocol}//${hostname}${port}/storages/` + this.currentTreeNode.storageId + '/' + this.currentTreeNode.repositoryId + '/download/' + result + params;
            window.open(baseUrl)
          }
        }
      } else if (active.key === '8') {
        const self = this;
        const file = this.currentTreeNode.artifactPath.split('/').pop();
        const targetFile = this.currentTreeNode.artifactPath.replace(".trash/", "");
        this.$confirm({
          title: this.$t('Store.Restore') + ": " + file,
          content: this.$t('Store.RestoreConfirm', { targetRepositories: this.folibRepository.id, path: targetFile }),
          okText: this.$t('Store.Confirm'),
          cancelText: this.$t('Store.Cancel'),
          onOk() {
            return new Promise((resolve, reject) => {
              const response = self.restorePackageHandle()
              if (response) {
                self.$notification.success({
                  message: self.$t('Store.RestoreSuccessful')
                })
                self.reload()
                resolve();
              }
            }).catch(error => {
              console.log(error);
              self.$notification.error({
                message: self.$t('Store.RestoreFailed')
              })
              reject();
            });
          },
          onCancel() { },
        });
      }
    },

    handleRightClick(active) {
      this.handleMenuClick(active)
      if (active.key === '4') {
        this.deletePackageHandle();
      }
    },
    handleDownLoadDir() {
      let url = this.currentTreeNode.url
      if (url) {
        url = url.replace("api/browse", "storages")
        window.open(url)
      }
      this.downLoadVisible = false;
    },
    handleDownLoadDirCancel() {
      this.downLoadVisible = false;
    },
    handleArchitectureMessage(message) {
      this.targetArchitecture = message;
    },
    getArtifactoryRepositoryType(key) {
      let artifactoryRepositoryType = ''
      this.externalNodeRepositories.forEach(node => {
        let arr = node.children.filter(i => i.key === key)
        if (arr && arr.length > 0) {
          artifactoryRepositoryType = arr[0].artifactoryRepositoryType
        }
      })
      return artifactoryRepositoryType
    },
    handleOperationSubmit(e) {
      e.preventDefault()
      this.operationForm.validateFields((err, values) => {
        if (!err) {
          let targetRepositoyList = []
          let targetDispatchRepositoryList = []
          values.targetRepositories.forEach(item => {
            let split = item.split(',')
            let arrayLength = split.length
            if (this.operationTitle.indexOf(this.$t('Store.Distribute')) !== -1) {
              let json = {}
              if (this.artifactoryType === 1) {
                let dispatchClusterEnName = split[0]
                let dispatchTargetStorageId = split[1]
                let dispatchTargetReopsitoryId = ''
                if (arrayLength === 3) {
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
            } else {
              targetRepositoyList.push({
                targetStorageId: split[0],
                targetRepositoryId: split[1]
              })
            }
          })
          let data = {
            path: values.path,
            targetPath: values.targetPath,
            srcStorageId: this.folibRepository.storageId,
            srcRepositoryId: this.folibRepository.id,
            targetRepositoyList: targetRepositoyList
          }
          let dispatchData = {
            path: values.path,
            targetPath: values.targetPath,
            srcStorageId: this.folibRepository.storageId,
            srcRepositoryId: this.folibRepository.id,
            targetDispatchRepositoryList: targetDispatchRepositoryList,
            type: this.folibRepository.type,
            layout: this.folibRepository.layout,
            policy: this.folibRepository.policy
          }
          if (this.operationTitle.indexOf(this.$t('Store.Copy')) !== -1) {
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
          } else if (this.operationTitle.indexOf(this.$t('Store.Move')) !== -1) {
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
          } else if (this.operationTitle.indexOf(this.$t('Store.Distribute')) !== -1) {
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
    operationFormModalClose() {
      this.showOperationFormModal = false
      this.showOperationDispatchFormModal = false
    },
    getArtifactDispatchStoragesAndRepositories(
      type,
      layout,
      policy
    ) {
      getArtifactDispatchStoragesAndRepositories({
        type: type,
        layout: layout,
        policy: policy
      }).then(res => {
        this.repositories = []
        res.forEach(item => {
          if (item.children && item.children.length > 0) {
            this.repositories.push(item)
          }
        })
        this.repositories = this.repositories
      })
    },
    queryPermissionStoragesAndRepositories(
      type,
      layout,
      excludeRepositoryId,
      policy
    ) {
      getPermissionStoragesAndRepositories({
        type: type,
        layout: layout,
        excludeRepositoryId: '',
        policy: policy
      }).then(res => {
        this.repositories = []
        res.forEach(item => {
          if (item.children && item.children.length > 0) {
            this.repositories.push(item)
          }
        })
      })
    },
    getMetadataConfiguration() {
      getMetadataConfiguration()
        .then(res => {
          this.metadataConfigList = res
        })
        .finally(() => { })
    },
    metadataHandler(type, metadata) {
      this.metadataFormReset()
      if (metadata) {
        this.metadataForm = metadata
      }
      this.handlerMetadataType = type
      this.showMetadataHandler = true
      this.getMetadataConfiguration()
    },
    metadataFormReset() {
      if (this.$refs.metadataForm) {
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
    dispatchPackageHandle() {
      console.log('分发处理 todo')
    },
    deletePackageHandle() {
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
          if (!msg || msg.length === 0 || typeof msg === 'object') {
            msg = this.$t('Store.DeletionFailed')
          }
          this.$notification.error({
            message: msg,
            description: ''
          })
        })
        .finally(() => { })
    },

    async restorePackageHandle() {
      const response = await restore(
        this.currentTreeNode.storageId,
        this.currentTreeNode.repositoryId,
        this.currentTreeNode.artifactPath
      );
      return response;
    },
    handlerRespMetadata(res) {
      let metadataList = []
      if (
        res.artifact &&
        res.artifact.metadata &&
        res.artifact.metadata.length > 0
      ) {
        let metadataJson = JSON.parse(res.artifact.metadata)
        for (let key in metadataJson) {
          let flag = this.metadataConfigList.some(
            metadataConfig =>
              !metadataConfig.viewShow && metadataConfig.key === key
          )
          if (flag) {
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
    metadataEditorDrawerShow(metadata) {
      this.metadataEditorDrawerTitle = metadata.key
      this.metadataEditorDrawerValue = metadata.value
      this.metadataEditorDrawerVisible = true
    },
    metadataEditHandler(metadata) {
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
      if (!flag) {
        data.custom = true
        data.customKey = key
      } else {
        data.key = key
        data.custom = false
      }
      this.metadataHandler(2, data)
      this.metadataTypeChange(data.type)
    },
    metadataTypeChange(value) {
      let editorList = ['TEXT', 'MD']
      let prismEditorList = ['JSON']
      let numberList = ['NUMERICAL']
      if (editorList.indexOf(value) !== -1) {
        this.metadataEditor = true
        this.metadataInput = false
        this.metadataNumber = false
        this.prismEditor = false
      } else if (prismEditorList.indexOf(value) !== -1) {
        this.prismEditor = true
        this.metadataInput = false
        this.metadataNumber = false
        this.metadataEditor = false
      } else if (numberList.indexOf(value) !== -1) {
        if (this.handlerMetadataType === 1) {
          this.metadataForm.value = undefined
        }
        this.metadataNumber = true
        this.metadataInput = false
        this.prismEditor = false
        this.metadataEditor = false
      } else {
        this.metadataInput = true
        this.metadataEditor = false
        this.metadataNumber = false
        this.prismEditor = false
      }
    },
    metadataPrismEditorDrawerShow(metadata) {
      this.metadataPrismEditorDrawerTitle = metadata.key
      this.metadataPrismEditorDrawerValue = metadata.value
      this.metadataPrismEditorDrawerVisible = true
    },
    changeCodeTye(item) {
      if (item) {
        this.codeParam = {
          type: item.name === 'Maven 2' ? 'maven' : item.name.toLowerCase(),
          code: item.code
        }
      }
    },
    getFileType(name) {
      if (name) {
        return getFileType(name)
      }
    },
    closeUsedVisibleDialog() {
      this.usedVisible = false
    },
    viewCodeHandle() {
      if (this.folibRepository.layout !== 'Docker' && this.currentFileDetial && !this.currentFileDetial.listTree) {
        if (this.currentFileDetial.artifact) {
          previewArtifact(this.currentTreeNode.storageId, this.currentTreeNode.repositoryId, this.currentTreeNode.artifactPath).then(res => {
            if (res && res.length > 0) {
              this.currentFileDetial.listTree = res
              this.$forceUpdate()
            } else {
              let len = this.currentFileDetial.artifact.sizeInBytes
              if (len && len > 1048576) {
                this.viewCodes = this.$t('Store.CannotPreview')
              } else {
                this.viewArtifactFile()
              }
            }
          })
        } else {
          this.viewArtifactFile()
        }
      }
      this.viewCodeVisible = true
    },
    viewArtifactFile() {
      viewArtifactFile(this.currentTreeNode.url).then(res => {
        if ('string' === typeof res && res.startsWith('PK')) {
          this.viewCodes = undefined
        } else if ('object' === typeof res) {
          if (res.data) {
            if ('string' === typeof res.data) {
              if (res.data.startsWith('PK')) {
                this.viewCodes = this.$t('Store.CannotPreview')
              } else {
                this.viewCodes = res.data
              }
            } else {
              this.viewCodes = JSON.stringify(res.data)
            }
          } else {
            this.viewCodes = JSON.stringify(res)
          }
        } else {
          this.viewCodes = res
        }
      })
    },
    closeViewCodeDialog() {
      this.viewCodeVisible = false
      this.viewCodes = null
    },
    metadataHandlerCancel() {
      this.metadataFormReset()
      this.showMetadataHandler = false
    },
    metadataReflesh() {
      this.metadataFormReset()
      this.$refs.BaseData.getMetadata()
      this.showMetadataHandler = false
    },
    search(value, searchType, type) {
      this.isSearch = true
      this.$nextTick(() => {
        this.$refs.search.search(value, searchType, type)
      })
    },
    onPageSizeChange() {
      this.search(this.artifactQuery.artifactName, 1)
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
          if (sorter.order.indexOf('desc') !== -1) {
            this.artifactQuery.sortOrder = 'desc'
          }
        }
      }
      this.search(this.artifactQuery.artifactName)
    },
    dateChange(value, dateString) {
      if (dateString) {
        this.artifactQuery.beginDate = dateString[0]
        this.artifactQuery.endDate = dateString[1]
        if (
          this.artifactQuery.beginDate === '' &&
          this.artifactQuery.endDate === ''
        ) {
          this.dateConfirm()
        }
      }
    },
    dateConfirm() {
      this.search(this.artifactQuery.artifactName, 1)
    },
    openDetial() {
      this.$emit('openDetial', this.scanReport)
    },
    highlighterHandle(code) {
      return highlight(code, languages.js) //returns html
    },
    fileSizeConver(size) {
      if (size) {
        return fileSizeConver(size)
      }
    },
    isAdmin() {
      return isAdmin()
    },
    queryStorageAndRepositoryPermission() {
      this.storageAdmin = ""
      this.permissions = []
      getStorageAndRepositoryPermission(
        this.folibRepository.storageId,
        this.folibRepository.id
      ).then(res => {
        this.storageAdmin = res.storageAdmin
        this.permissions = res.permissions
        this.uploadEnabled =
          this.folibRepository.status.indexOf('Out of Service') === -1 &&
          this.enablUploadedLayout.includes(this.folibRepository.layout) &&
          (this.folibRepository.type === 'hosted' || (this.folibRepository.type === 'group' && this.folibRepository.groupDefaultRepository))
        console.log("this.uploadEnabled", this.uploadEnabled)
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
    getRepositoryUrl() {
      let repositoryUrl = ''
      if (this.baseUrl) {
        repositoryUrl =
          this.baseUrl +
          'storages/' +
          this.folibRepository.storageId +
          '/' +
          this.folibRepository.id
        let layout = this.folibRepository.layout.toLowerCase()
        if (layout === 'docker') {
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
    handleMavenUpload() {
      this.mavenUploadVisible = true
    },
    mavenUploadClose() {
      this.mavenUploadVisible = false
    },
    uploadTypeChange(element) {
      if (element.target.value === 1) {
        this.uploadType = 1
      } else if (element.target.value === 2) {
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
    getExternalNodeRepositories(params) {
      getExternalNodeRepositories(params).then(res => {
        if (res) {
          res.forEach(node => {
            let json = { key: node.key, artifactoryRepositoryType: '', children: [], }
            node.repositories.forEach(repo => {
              json.children.push({ key: repo.key, artifactoryRepositoryType: repo.artifactoryRepositoryType, children: null })
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
    },
    handleDebianUpload() {
      this.$refs.debianmodal.openModal();
    },
    handleDebianBatchUpload() {
      this.$refs.debianBatchModal.openModal();
    },
    onFileChange(event) {
        //console.log('onFileChange', event)
        this.selectedFiles = Array.from(event.fileList.map(file => file.originFileObj)); // 将文件存储为数组
          this.selectedFiles.forEach((file, index) => {
              this.totalChunks[index] = Math.ceil(file.size / this.chunkSize);
              this.fileIds[index] = this.generateFileId(file);// 根据文件生成唯一ID
              this.uploadProgresses[index] = 0; // 初始化进度
              this.currentChunks[index] = 0; // 初始化当前分片
              //this.fileMD5s[index] = ''; // 初始化文件 MD5
              //this.md5Progresses[index] = 0; // 初始化 MD5 计算进度
              //this.calculateMD5(file, index); // 计算文件 MD5
          });
        // 开始计算 MD5
        this.calculateFilesMD5();
        // 计算所有文件的总大小
        this.totalUploadSize = this.selectedFiles.reduce((sum, file) => sum + file.size, 0);
    },

      // 计算多个文件的 MD5 值
      async calculateFilesMD5() {
          this.md5CalculationComplete = false; // 标记 MD5 计算为未完成
          this.md5Progresses = new Array(this.selectedFiles.length).fill(0); // 初始化进度为 0
          //console.log("md5Progresses:",this.md5Progresses)
          const md5Promises = this.selectedFiles.map((file, index) => this.calculateMD5(file, index));

          // 使用 Promise.all 等待所有文件的 MD5 计算完成
          await Promise.all(md5Promises);
          // 所有 MD5 计算完成，启用上传按钮
          this.md5CalculationComplete = true;

      },

      /**
       * 计算每个文件的 MD5 值
       * @param file
       * @param index
       * @returns {Promise<unknown>}
       */
      calculateMD5(file, index) {
          return new Promise((resolve) => {
              const chunkSize = 2 * 1024 * 1024; // 每次读取2MB
              const chunks = Math.ceil(file.size / chunkSize);
              let currentChunk = 0;
              const spark = new SparkMD5.ArrayBuffer();
              const fileReader = new FileReader();

              fileReader.onload = (e) => {
                  spark.append(e.target.result); // 添加数据到 SparkMD5
                  currentChunk++;

                  if (currentChunk < chunks) {
                      this.md5Progresses[index] = Math.floor((currentChunk / chunks) * 100); // 更新 MD5 计算进度
                      loadNext();
                  } else {
                      this.fileMD5s[index] = spark.end(); // 计算最终的 MD5
                      this.md5Progresses[index] = 100;
                      resolve()
                  }
              };

              fileReader.onerror = () => {
                  console.error('文件读取错误');
                  resolve()
              };

              const loadNext = () => {
                  const start = currentChunk * chunkSize;
                  const end = Math.min(start + chunkSize, file.size);
                  fileReader.readAsArrayBuffer(file.slice(start, end));
              };

              loadNext(); // 开始读取第一个分片
          });
      },
      generateFileId(file) {
          return `${file.name}-${file.size}-${file.lastModified}`;
      },

      /**
       * 上传文件
       * @param path 路径
       * @param isUnzip 是否解压
       * @param fileMetaDataMap 文件元数据
       * @param imageTag 镜像tag
       * @param fileType 文件类型 （业务类型）
       * @returns {Promise<void>}
       */
      async uploadFiles(path,isUnzip,fileMetaDataMap,imageTag,fileType) {
          this.isUploading = true;
          this.paused = false;
          this.currentFileIndex = 0; // 重置当前上传文件的索引
          this.uploadedSize = 0; // 重置已上传的大小
          await this.uploadNextFile(path,isUnzip,fileMetaDataMap,imageTag,fileType); // 开始上传文件
      },
      /**
       * 上传下一个文件
       * @param path 路径
       * @param isUnzip 是否解压
       * @param fileMetaDataMap 文件元数据
       * @param imageTag 镜像tag
       * @param fileType 文件类型 （业务类型）
       * @returns {Promise<void>}
       */
      async uploadNextFile(path,isUnzip,fileMetaDataMap,imageTag,fileType) {
          // 当有文件未上传时，继续上传下一个文件
          if (this.currentFileIndex < this.selectedFiles.length) {
              const file = this.selectedFiles[this.currentFileIndex];
              await this.uploadFile(file, this.currentFileIndex,path,isUnzip,fileMetaDataMap,imageTag,fileType);
              this.currentFileIndex++; // 文件上传完成后处理下一个文件
              this.uploadNextFile(path,isUnzip,fileMetaDataMap,imageTag,fileType); // 递归上传下一个文件
          } else {
              this.isUploading = false; // 所有文件上传完成
          }
      },
      /**
       * 上传文件
       * @param file 文件对象
       * @param fileIndex 文件索引
       * @param path 路径
       * @param isUnzip 是否解压
       * @param fileMetaDataMap 文件元数据
       * @param imageTag 镜像tag
       * @param fileType 文件类型 （业务类型）
       * @returns {Promise<void>}
       */
      async uploadFile(file, fileIndex,path,isUnzip,fileMetaDataMap,imageTag,fileType) {
          const promises = [];
          for (this.currentChunks[fileIndex]; this.currentChunks[fileIndex] < this.totalChunks[fileIndex]; this.currentChunks[fileIndex]++) {
              if (this.paused) break;
              if (this.activeUploads >= this.maxConcurrency) {
                  await Promise.race(promises); // 等待至少一个上传完成
              }
              promises.push(this.uploadSingleChunk(file, fileIndex, this.currentChunks[fileIndex],path,isUnzip,fileMetaDataMap,imageTag,fileType));
          }

          await Promise.all(promises); // 等待所有分片上传完成
      },

      /**
       * 上传单个分片
       * @param file 文件
       * @param fileIndex 文件索引
       * @param chunkIndex 切片索引
       * @param path 路径
       * @param isUnzip 是否解压
       * @param fileMetaDataMap 文件元数据
       * @param imageTag 镜像tag
       * @param fileType 文件类型 （业务类型）
       * @returns {Promise<unknown>}
       */
      uploadSingleChunk(file, fileIndex, chunkIndex, path,isUnzip,fileMetaDataMap,imageTag,fileType) {
          return new Promise(async (resolve, reject) => {
              this.activeUploads++;
              const start = chunkIndex * this.chunkSize;
              const end = Math.min(start + this.chunkSize, file.size);
              const chunk = file.slice(start, end);
              const chunkSize = end - start;
              const chunkMD5 = SparkMD5.ArrayBuffer.hash(chunk);
              const formData = new FormData();
              formData.append('file', chunk, file.name);
              formData.append('fileName', file.name);
              formData.append('fileId', this.fileIds[fileIndex]);
              formData.append('storageId', this.folibRepository.storageId);
              formData.append('repositoryId', this.folibRepository.id);
              formData.append('path', path);
              formData.append('totalChunks', this.totalChunks[fileIndex]);
              formData.append('currentChunk', chunkIndex + 1);
              formData.append('currentChunkSize', chunk.size); // 添加当前分片大小
              formData.append('chunkSize', chunkSize);
              formData.append('chunkMD5', chunkMD5);
              formData.append('fileMd5', this.fileMD5s[fileIndex]);
              formData.append("originalFilename",file.name)
              formData.append('isUnzip', isUnzip);
              formData.append('fileMetaDataMap', fileMetaDataMap);
              formData.append('imageTag', imageTag ? imageTag:'');
              formData.append('fileType', fileType);
              // 发送请求
              try {
                  await this.uploadChunk(formData, fileIndex, chunkIndex,chunkSize);
                  this.progressStatus='active';
                  resolve();
              } catch (error) {
                  reject(error);
                  this.progressStatus = 'exception';
                  this.$notification['error']({
                      message: this.$t('Store.UploadFailed'),
                      description: ''
                  })
              } finally {
                  this.activeUploads--;
              }
          });
      },
      uploadChunk(formData, fileIndex, chunkIndex,chunkSize) {
          return new Promise((resolve, reject) => {
              const xhr = new XMLHttpRequest();
              xhr.open('POST', '/api/artifact/folib/promotion/slice/upload-web');
              const token = storage.get(ACCESS_TOKEN) ? storage.get(ACCESS_TOKEN) : Cookies.get("access_token");
              xhr.setRequestHeader('Authorization', 'Bearer '+token);
              xhr.upload.onprogress = (event) => {
                  if (event.lengthComputable) {
                      this.isClose = true;
                      this.uploadProgresses[fileIndex] = Math.floor(
                          ((chunkIndex + event.loaded / event.total) / this.totalChunks[fileIndex]) * 100
                      );

                      //const uploadedChunkSize = event.loaded / event.total * chunkSize;
                      //console.log("totalChunks:", this.totalChunks[fileIndex],"event.loaded:",event.loaded,"event.total:",event.total,"uploadedChunkSize:",uploadedChunkSize)
                      //this.updateTotalProgress(uploadedChunkSize);
                  }
              };
              xhr.onload = () => {

                  if (xhr.status === 200) {
                      this.updateTotalProgress(chunkSize); // 上传完成时更新总进度
                      resolve();
                  } else {
                      this.progressStatus = 'exception'
                      //console.log('upload status:',xhr.status,'xhr.responseText:',xhr.responseText,"xhr.statusText",xhr.statusText)
                      reject(xhr.responseText);

                  }
              };
              xhr.onerror = () => reject('上传失败');
              xhr.send(formData);
          });
      },
      updateTotalProgress(uploadedChunkSize) {
          // 更新已上传的总大小
          this.uploadedSize += uploadedChunkSize;
          // 更新总的上传进度百分比
          this.totalUploadProgress = Math.floor((this.uploadedSize / this.totalUploadSize) * 100);
          if( this.totalUploadProgress ===100){
              this.progressStatus = 'success';
          }
          //console.log("uploadedChunkSize:", uploadedChunkSize,"totalUploadProgress:", this.totalUploadProgress,"totalUploadSize:", this.totalUploadSize,"uploadedSize:", this.uploadedSize)
      },
      pauseUpload() {
          this.paused = true;
          this.isUploading = false;
      },
      resumeUpload() {
          if (this.paused) {
              this.isUploading = true;
              this.paused = false;
              this.uploadNextFile(); // 恢复文件上传
          }
      },
      onCloseSpeed(){
        this.isClose = false;
      },
      getFormattedUrl(url) {
        if (!url) return '';
        // 替换 localhost:38080 为实际的 baseUrl
        if (url.includes('http://localhost:38080/')) {
          return url.replace('http://localhost:38080/', this.baseUrl);
        }
        return url;
      },
     onRightClick(params) {
       this.showContextMenu = true;
       this.rightClickTop = `${params.event.clientY}px`;
       this.rightClickLeft = `${params.event.clientX}px`;
       this.currentTreeNode = params.node.dataRef;
     },
     closeContextMenu() {
       this.showContextMenu = false;
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

.ellipsis-text {
  white-space: nowrap;
  // overflow: hidden;
  text-overflow: ellipsis;
}


.ellipsis-link {
  max-width: calc(100% - 50px);
  display: inline-block;
  white-space: nowrap;
  text-overflow: ellipsis;
  vertical-align: bottom;
  overflow: hidden;
}

.context-menu {
  z-index: 1000;
  background-color: #fff;
  border: 1px solid #d9d9d9;
  border-radius: 4px;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);

  /deep/ .ant-menu-item {
    margin: 0;
    height: 35px;
    line-height: 35px;
    padding: 0 8px;
  }
}

.view-switch {
  cursor: pointer;
}
</style>
