<!--
	This is the Settings page, it uses the dashboard layout in:
	"./layouts/Dashboard.vue" .
 -->

 <template>
  <div id="storages">
    <a-row :style="isChecked ? 'opacity:1;' : 'opacity:0;'" type="flex" :gutter="[24, 24]" style="margin-bottom:-30px;transition: all 0.5s ease;">
        <a-col :span="24" :lg="24">
          <storageInfoCard
            ref="storageInfoCard"
            :currentStorage="currentStorage"
            :baseUrl="baseUrl"
            :layoutType="layoutType"
            :storageData="storageData"
            @copy="copy"
            @updateHandleView="updateHandleView"
            @handheTableSearch='handheTableSearch'
            @createHandleView="createHandleView"
            @showOverview="showOverview"
          />
        </a-col>
    </a-row>
    <a-row :style="isChecked ? 'margin-top:0;' : 'margin-top:-120px;'" style="transition: all 0.5s ease;margin-bottom:-30px;" v-if="!isShowOverview" type="flex" :gutter="[24, 24]">
      <a-col :span="24" :lg="6">
        <!-- Page Anchors -->
<!--        <a-affix :offset-top="navbarFixed ? 100 : 10">-->
          <a-card :bordered="false" :style="isChecked ? 'height:calc(100vh - 230px);margin-bottom:0px;' : ''" class="header-solid mb-24 left_menu">
            <template #title>
              <a-row type="flex" align="middle" class="position:relative;">
                <a-col :span="24" :md="14" class="col-info" style="display: flex; align-items: center;">
                  <h6 class="font-semibold m-0">{{ isChecked ? $t('Storage.RepositoryList') : $t('Storage.StorageList')}}</h6>
                  <a style="margin-left: 15px;" class="text-center text-muted font-bold" v-if="!isChecked" :title="$t('Storage.CreateStorageSpace')">
                    <h3 v-if="$store.state.user.roles.indexOf('ADMIN') > -1" class="font-semibold text-muted mb-0"  @click="createHandleView">
                      +
                    </h3>
                  </a>
                  <a style="margin-left: 15px;" class="text-center text-muted font-bold" v-if="isChecked" :title="$t('Storage.CreateRepository')">
                    <h3 class="font-semibold text-muted mb-0" @click="folibVisibleShow">
                      +
                    </h3>
                  </a>
                </a-col>
                <a-col :span="24" :md="10" style="display: flex; align-items: center; justify-content: flex-end">
                  <!-- <a-switch 
                    :disabled="switchDisabled"
                    style="margin-right:10px;"
                    v-model="isChecked"
                    class="switch-position"
                    @change="getDetailInfo"
                  ></a-switch> -->
                    <!-- <a class="text-center text-muted font-bold" v-if="isChecked" style="margin-right:8px;">
                      <h5 class="font-semibold text-muted mb-0"
                        @click="toggleTree">
                          <a-icon v-if="!isTrashView" type="delete" />
                          <a-icon v-else type="file-zip" />
                      </h5>
                    </a> -->
                    <div class="switch_mode">
                    <div @click="checkMode(false)" class="img-sty" :class="isChecked ? '' : 'isActive'">
                      <img src="./images/list.svg" width="20" alt="">
                    </div>
                    <div @click="checkMode(true)" class="img-sty" :class="isChecked ? 'isActive' : ''">
                      <img src="./images/tree.svg" width="20" alt="">
                    </div>
                    <div :style="switchDisabled?'display:block;':'display:none;'" class="disabled_sty"></div>
                  </div>
                </a-col>
              </a-row>
            </template>
            <!-- 仓库列表树 -->
            <repositoryTree 
              v-if="isChecked"
              ref="repositoryTree" 
              @loadMore="loadMore" 
              @handleMenuClick="handleMenuClickTree"
              @treeSelect="treeSelect" 
              @repositorySelect="repositorySelect" 
              @expand="onExpand" 
              @getDetailInfo="getDetailInfo"
              :repositories="repositories" 
              :isTrashView="isTrashView"
              :storageId="currentStorage.id" 
            />
            <!-- 存储列表 -->
            <storageList 
                v-else 
                ref="storageList"
                :navbarFixed="navbarFixed" 
                :storageData="storageData" 
                :currentStorage="currentStorage"
                @setCurrentStorage="setCurrentStorage" 
            />
          </a-card>
<!--        </a-affix>-->
        <!-- / Page Anchors -->

      </a-col>
      <a-col :span="24" :lg="18">
        <storageInfoCard
          :style="isChecked ? 'opacity:0;' : 'opacity:1;'"
          style="transition: all 0.5s ease;"
          :currentStorage="currentStorage"
          :baseUrl="baseUrl"
          @copy="copy"
          @updateHandleView="updateHandleView"
          :type="'noFilter'"
        />
        <a-tabs v-if="!isChecked" class="tabs-sliding self_tabs" style="margin-top:-20px;" default-active-key="1">
          <a-tab-pane key="1" :tab="$t('Storage.RepositoryList')">
            <div style="height:calc(100vh - 270px);overflow-y: auto;overflow-x: hidden;border-radius: 12px;margin-top:0px;padding-left:4px;padding-right: 10px;">
              <a-row type="flex" :gutter="20">
                <a-col :span="8" style="margin-bottom:20px;" v-if="hasStoragePermission()">
                  <a-card @click="folibVisibleShow()" class="crm-bar-line header-solid h-full xinjian"
                    :bodyStyle="{ padding: 0, height: '100%', display: 'flex', alignItems: 'center', justifyContent: 'center' }">
                    <a class="text-center text-muted font-bold">
                      <h3 class="font-semibold text-muted mb-0">+</h3>
                      <h5 class="font-semibold text-muted">{{ $t('Storage.createModal') }}</h5>
                    </a>
                  </a-card>
                </a-col>
                <a-col :span="8" style="margin-bottom:20px;" v-for="(item, index) in repositories" :key="index">
                  <!-- Project Card -->
                  <CardProjectFolib :title=item.id :logo="'images/folib/' + getLayoutType(item) + '.svg'"
                    :team="['images/folib/' + item.type + '.svg']" :participants="item.type" :due="item.policy"
                    :repository="item" :storageAdmin="currentStorage.admin" @handleMenuClick="handleMenuClick" @goToDetial="goToDetial(item)">
                    <a-tooltip placement="topLeft">
                      <template slot="title">
                        {{ getRepositoryUrl(item) }}
                      </template>
                      <p style="margin-left: 75px;">http://..../{{ item.id }} <a>
                          <a-icon type="copy" @click="copy(getRepositoryUrl(item))" />
                        </a></p>
                    </a-tooltip>
                  </CardProjectFolib>
                  <!-- / Project Card -->
                </a-col>
              </a-row>
            </div>
            <!-- 分页 -->
            <div class="fenye">
              <a-pagination @change="handlerPageNum" @showSizeChange="handlerPageSize" :total="queryParams.total" show-size-changer />
            </div>
          </a-tab-pane>
          <a-tab-pane key="2" v-if="isLogin" :tab="$t('Storage.StorageOverview')">
            <a-row type="flex" :gutter="24" style="height: calc(100vh - 270px);overflow:auto;margin-top:0px;padding-left:4px;margin-right: 4px;">
              <a-col :span="24">
                <Overview :storageId="currentStorage.id"/>
                <StorageInfo class="mt-20" :storageId="currentStorage.id"/>
              </a-col>
            </a-row>
          </a-tab-pane>
        </a-tabs>
        <!-- 存储空间模式下 直接展示仓库内容 -->
        <LibView 
          v-else
          :key="libViewKey"
          ref="libview" 
          @reloadTree="reloadTree"
          @handleMenuClick="handleMenuClick"
          :storageAdmin="currentStorage.admin" 
          :style="isChecked ? 'margin-top:-105px;' : ''" style="border:none;transition: all 0.5s ease;" 
          :isChecked="isChecked" 
        />
      </a-col>
    </a-row>
    <!-- 存储空间模式下 切换到存储概览 -->
    <a-row v-else style="margin-top:20px;">
      <a-col :span="24">
        <Overview :storageId="currentStorage.id"/>
        <StorageInfo class="mt-20" :storageId="currentStorage.id"/>
      </a-col>
    </a-row>

    <a-modal v-model="showsTorageFormModal" :footer="null" :forceRender="true" :title="$t('Storage.CreateStorageSpace')"
      on-ok="showsTorageFormModal = false">
      <a-form-model :model="storageCreateData" ref="storageCreate" :rules="storageRules" :hideRequiredMark="true"
        @submit.prevent="handleCreateSubmit">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-model-item class="tags-field mb-10" :label="$t('Storage.StorageSpaceName')" :colon="false" prop="id">
              <a-input v-decorator="['id']" v-model="storageCreateData.id" :placeholder="$t('Storage.StorageSpaceName')">
                <a-icon slot="prefix" type="appstore" />
              </a-input>
            </a-form-model-item>
            <a-form-model-item class="mb-10"
                                                   :label="$t('Storage.SyncStorage')"
                                                   :colon="false" style="position: relative"
                                                   prop="syncEnabled">
            <a-switch v-model="storageCreateData.syncEnabled" />
            <a-tooltip :title="$t('Setting.SyncStoragePrompt')" class="info-message">
              <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
            </a-tooltip>
          </a-form-model-item>
            <a-form-model-item class="tags-field mb-10" :label="$t('Storage.StorageType')" :colon="false">
              <a-radio-group name="radioGroup" default-value="local" @change="changeStorageType()"
                v-model="storageCreateData.storageProvider">
                <a-radio value="local">
                  {{ $t('Storage.LocalStorage') }}
                </a-radio>
                <a-radio value="s3">
                  {{ $t('Storage.s3Storage') }}
                </a-radio>
              </a-radio-group>
            </a-form-model-item>
            <p>{{ $t('Storage.Note') }}：</p>
            <ul class="pl-15 text-muted">
              <li>{{ $t('Storage.NFSStorage') }}</li>
              <li>{{ $t('Storage.BucketNameDefinition') }}</li>
              <li><strong>{{ $t('Storage.unmodifiableNote') }}</strong></li>
            </ul>
            <a-form-model-item class="tags-field mb-10" :label="storageCreateData.storageProvider=='local'?$t('Storage.LocalPath'):$t('Storage.S3Path')"
              :colon="false">
              <a-card :bordered="false" class="bg-gray-3 shadow-0 mb-24" :bodyStyle="{ padding: '8px' }">
                <a-row type="flex" align="middle">
                  <a-col>
                    <strong class="font-semibold">{{
                      storagePrefix ? '/' + storagePrefix : storageCreateData.storageProvider ==='local' ? '/storages' : ''
                    }}/{{ storageCreateData.id }}</strong>
                  </a-col>
                  <a-col class="ml-auto">
                    <a-input v-if="customStorage" v-model="storagePrefix" :placeholder="storageCreateData.storageProvider ==='s3' ? $t('Storage.BucketName') : $t('Storage.ParentDirectory')"
                      class="font-regular text-sm text-dark" style="width: 150px;">
                      <a-icon slot="prefix" type="cloud" v-if="storageCreateData.storageProvider ==='s3'"/>
                      <a-icon slot="prefix" type="appstore" v-else/>
                    </a-input>
                    <a-button v-if="!customStorage"
                      @click="() => (customStorage = true)" size="small" type="link"
                      class="ml-10 px-25 font-bold">{{ $t('Storage.Custom') }}
                    </a-button>
                    <a-button v-if="customStorage"
                      @click="() => (customStorage = false, storagePrefix = null)"
                      size="small" type="link" class="ml-10 px-25 font-bold">{{ $t('Storage.cancelCustom') }}
                    </a-button>
                  </a-col>
                </a-row>
              </a-card>
            </a-form-model-item>
            <a-form-model-item class="mb-10" :label="$t('Storage.StorageSizeLimit')" :colon="false">
                <a-input v-model="storageMaxSize" addon-after="TB">
                </a-input>
            </a-form-model-item>
            <a-form-model-item class="tags-field mb-10" v-if="userInfo.roles.indexOf('ADMIN') > -1" :label="$t('Storage.Administrator')"
              :colon="false">
              <a-select v-model="storageCreateData.admin" style="width: 100%" model="default" show-search allowClear
                :dropdown-style="{ maxHeight: '240px', overflow: 'auto' }"
                @change="userOnChange"
                @search="userSearchChange"
                @popupScroll="userPopupScroll"
                :getPopupContainer="(triggerNode) => triggerNode.parentNode || document.body"
                :filter-option="false"
                :placeholder="$t('Storage.SelectAdministrator')">
                <a-select-option v-for="(username, index) in userList" :key="index" :value="username">
                  {{ username }}
                </a-select-option>
              </a-select>
            </a-form-model-item>
<!--            <a-form-model-item class="tags-field mb-10"
              v-if="hasStoragePermission()" :label="$t('Storage.user')"
              show-search :colon="false">
              <a-select v-model="storageCreateData.users" mode="multiple" :defaultValue="storageCreateData.users"
                style="width: 100%" :placeholder="$t('Storage.selectUser')">
                <a-select-option v-for="(tag, index) in userList" :key="index" :value="tag.username">
                  {{ tag.username }}
                </a-select-option>
              </a-select>
            </a-form-model-item>-->
            <a-form-model-item class="mb-10" :colon="false">

            </a-form-model-item>
          </a-col>
          <a-col :span="12">
            <!--            <a-button key="back" @click="deleteCurrentTask" class="px-30" size="small" type="danger">Delete</a-button>-->
          </a-col>
          <a-col :span="12" class="text-right">
            <a-button key="submit" class="px-30" size="small" type="primary" htmlType="submit">{{ $t('Storage.Confirm') }}</a-button>
            <a-button key="back" @click="showsTorageFormModal = false" class="px-30 ml-10" size="small">{{ $t('Storage.Cancel') }}</a-button>
          </a-col>
        </a-row>
      </a-form-model>
    </a-modal>

    <a-modal v-model="showStorageUpdate" :footer="null" :forceRender="true" :title="$t('Storage.StorageSpaceOperation')"
      on-ok="showStorageUpdate = false">
      <a-form :hideRequiredMark="true">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-item class="tags-field mb-10" :label="$t('Storage.StorageSpaceName')" :colon="false">
              <a-input disabled v-model="currentStorage.id" :placeholder="$t('Storage.StorageSpaceName')">
                <a-icon slot="prefix" type="appstore" />
              </a-input>
            </a-form-item>
            <a-form-item class="mb-10"
                               :label="$t('Storage.SyncStorage')"
                               :colon="false" style="position: relative"
                               prop="syncEnabled">
              <a-switch v-model="currentStorage.syncEnabled" @change="changeSyncEnabled"/>&nbsp;&nbsp;
              <a-tooltip :title="$t('Setting.SyncStoragePrompt')" class="info-message">
                <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
              </a-tooltip>
            </a-form-item>
            <a-form-item class="tags-field mb-10" :label="$t('Storage.StorageType')" :colon="false">
              <a-radio-group disabled name="radioGroup" default-value="local" @change="changeStorageUpdateType()"
                v-model="currentStorage.storageProvider">
                <a-radio value="local">
                  {{ $t('Storage.LocalStorage') }}
                </a-radio>
                <a-radio value="s3">
                  {{ $t('Storage.s3Storage') }}
                </a-radio>
              </a-radio-group>
            </a-form-item>
            <p>Tip:</p>
            <ul class="pl-15 text-muted">
              <li>{{ $t('Storage.SpaceNameRemain') }}</li>
              <li>{{ $t('Storage.BucketRemain') }}</li>
            </ul>
            <a-form-item class="tags-field mb-10" :label="currentStorage.storageProvider=='local'?$t('Storage.LocalPath'):$t('Storage.S3Path')" :colon="false">
              <a-card :bordered="false" class="bg-gray-3 shadow-0 mb-24" :bodyStyle="{ padding: '8px' }">
                <a-row type="flex" align="middle">
                  <a-col>
                    <strong class="font-semibold">{{
                      storagePrefix ? '/' + storagePrefix : currentStorage.storageProvider ==='local' ? '/storages' : ''
                    }}/{{ currentStorage.id }}</strong>
                  </a-col>
                  <a-col class="ml-auto">
                    <a-input v-if="customStorage" v-model="storagePrefix" :placeholder="currentStorage.storageProvider ==='s3' ? $t('Storage.BucketName') : $t('Storage.ParentDirectory')"
                      class="font-regular text-sm text-dark" style="width: 150px;">
                      <a-icon slot="prefix" type="cloud" v-if="currentStorage.storageProvider === 's3'"/>
                      <a-icon slot="prefix" type="appstore" v-else />
                    </a-input>
                    <a-button disabled v-if="!customStorage"
                      @click="() => (customStorage = true)" size="small" type="link"
                      class="ml-10 px-25 font-bold">{{ $t('Storage.Custom') }}
                    </a-button>
                    <a-button v-if="customStorage"
                      @click="() => (customStorage = false, storagePrefix = null)" size="small"
                      type="link" class="ml-10 px-25 font-bold">{{ $t('Storage.cancelCustom') }}
                    </a-button>
                  </a-col>
                </a-row>
              </a-card>
            </a-form-item>
            <a-form-item class="mb-10" :label="$t('Storage.StorageSizeLimit')" :colon="false">
                <a-input v-model="storageMaxSize" addon-after="TB">
                </a-input>
            </a-form-item>
            <a-form-item class="tags-field mb-10" v-if="userInfo.roles.indexOf('ADMIN') > -1" :label="$t('Storage.Administrator')"
              :colon="false">
              <a-select v-model="currentStorage.admin" style="width: 100%" model="default" show-search allowClear
                :dropdown-style="{ maxHeight: '240px', overflow: 'auto' }"
                @change="userOnChange"
                @search="userSearchChange"
                @popupScroll="userPopupScroll"
                :getPopupContainer="(triggerNode) => triggerNode.parentNode || document.body"
                :filter-option="false"
                :placeholder="$t('Storage.SelectAdministrator')">
                <a-select-option v-for="(username, index) in userList" :key="index+1" :value="username">
                  {{ username }}
                </a-select-option>
              </a-select>
            </a-form-item>
<!--            <a-form-item class="tags-field mb-10"
              v-if="hasStoragePermission()" :label="$t('Storage.user')"
              :colon="false">
              <a-select v-model="currentStorage.users" mode="multiple" :defaultValue="currentStorage.users"
                style="width: 100%" :placeholder="$t('Storage.selectUser')">
                <a-select-option v-for="(tag, index) in userList" :key="index" :value="tag.username">
                  {{ tag.username }}
                </a-select-option>
              </a-select>
            </a-form-item>-->
            <a-form-item class="mb-10" :colon="false">

            </a-form-item>
          </a-col>
          <a-col :span="12">

          </a-col>
        </a-row>
        <p>{{ $t('Storage.ImportantNote') }}:</p>
        <ul class="pl-15 text-muted">
          <li>{{ $t('Storage.HaveAccess') }}</li>
          <li>{{ $t('Storage.RegularCleaning') }}</li>
          <li>{{ $t('Storage.IfForcedDeletion') }}</li>
        </ul>
        <a-row :span="24">
          <a-col :span="12" class="text-left">
            <a-button @click="handleUpdateSubmit" class="px-30" size="small" type="primary" htmlType="submit">{{ $t('Storage.Edit') }}
            </a-button>
            <a-button @click="showStorageUpdate = false" class="px-30 ml-10" size="small">{{ $t('Storage.Cancel') }}</a-button>
          </a-col>
          <a-col :span="12" class="text-right">
            <a-button @click="storageDelHandle" class="px-30 ml-10" type="danger" size="small">{{ $t('Storage.Delete') }}</a-button>
            <a-button @click="storageForceDelHandle" class="px-30 ml-10" type="dashed" size="small">{{ $t('Storage.ForcedDeletion') }}</a-button>
          </a-col>
        </a-row>
      </a-form>
    </a-modal>
    <a-modal v-model="deleteFormVisible" :footer="null" :forceRender="true" on-back="deleteFormVisible = false">
      <a-form :form="delForm" ref="delForm" :hideRequiredMark="true">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <h6 class="text-center font-regular">{{ $t('Storage.ConfirmDeletion1') }}<a>{{ willDelId }}</a>{{ $t('Storage.ConfirmDeletion2') }}</h6>
          </a-col>
          <a-col :span="8">
          </a-col>
          <a-col :span="8">
            <a-form-item class="mb-10" :colon="false">
              <a-input v-decorator="['id',]" :placeholder="$t('Storage.WarehouseName')">
              </a-input>
            </a-form-item>
          </a-col>
          <a-col :span="8">
            <!--            <a-button key="back" @click="deleteCurrentTask" class="px-30" size="small" type="danger">Delete</a-button>-->
          </a-col>
        </a-row>
        <p>{{ $t('Storage.ImportantNote') }}:</p>
        <ul class="pl-15 text-muted">
          <li>{{ $t('Storage.LogicalDeletion') }}</li>
          <li>{{ $t('Storage.IfForcedDeletion') }}</li>
        </ul>
        <a-row :span="24">
          <a-col :span="12" class="text-left">
            <a-button key="back" @click="deleteFormVisible = false" class="px-30 ml-10" size="small">{{ $t('Storage.Cancel') }}</a-button>
          </a-col>
          <a-col :span="12" class="text-right">
            <a-button v-if="deleteBtnVisible" @click="delRepositoryResponseEntity" class="px-30 ml-10" type="danger"
              size="small">{{ $t('Storage.Delete') }}</a-button>
            <a-button v-if="forceDeleteBtnVisible" @click="delRepositoryResponseEntityForce" class="px-30 ml-10"
              type="dashed" size="small">{{ $t('Storage.ForcedDeletion') }}
            </a-button>
          </a-col>
        </a-row>
      </a-form>
    </a-modal>
    <a-drawer placement="right" width="65%" :title="(folibRepositoryEditDisabled ? $t('Storage.Edit') : $t('Storage.create')) + $t('Storage.ProductWarehouse')"
      :visible="folibVisible" @close="closeUserDialog">
      <div class="mx-auto m-50" style="max-width: 1000px;">
        <!-- Header -->
        <h3 class="mt-25 mb-5 text-center">{{ $t('Storage.Start') }}{{ folibRepositoryEditDisabled ? $t('Storage.Edit') : $t('Storage.create') }}{{ $t('Storage.yourFolib') }}</h3>
        <h5 v-if="language==='zh'" class="text-center font-regular">{{ $t('Storage.Will') }}
          <a>{{currentStorage.id }}</a>
          {{ $t('Storage.UnderStorageSpace') }}{{ folibRepositoryEditDisabled ? $t('Storage.Edit') : $t('Storage.create') }}{{ $t('Storage.ProductWarehouse') }}
        </h5>
        <h5 v-if="language==='en'" class="text-center font-regular">{{ $t('Storage.Will') }}
          {{ folibRepositoryEditDisabled ? $t('Storage.Edit') : $t('Storage.create') }} {{ $t('Storage.Under') }}
          <a>{{currentStorage.id }}</a> {{ $t('Storage.StorageSpace') }}
        </h5>
        <div class="my-50" style="max-width: 1000px;">

          <!-- Steps -->
            <a-steps progress-dot v-model="step" @change="handleStepChange" size="small" :status="stepsStatus">
                <a-step
                    v-if="folibRepository.type === 'hosted' || folibRepository.type === 'proxy' || folibRepository.type === 'group'"
                    :title="$t('Storage.TypeSelection')" style="margin-left: -10px;"/>
                <a-step disabled
                        v-if="folibRepository.type === 'hosted' || folibRepository.type === 'proxy' || folibRepository.type === 'group'"
                        :title="$t('Storage.BasicInformation')"/>
                <a-step v-if="folibRepository.type === 'proxy'" :title="$t('Storage.RemoteConfiguration')"/>
                <a-step v-if="folibRepository.type === 'group'" :title="$t('Storage.CombinationConfiguration')"/>
                <a-step :title="$t('Storage.PermissionSetting')" :disabled="isRepoExist" />
                <a-step :title="$t('Storage.TimingPolicy')" :disabled="isRepoExist" />
                <a-step :title="$t('Storage.FederatedRepository')"  :disabled="isRepoExist"/>
                <a-step :title="$t('Storage.Scan')"  :disabled="isRepoExist"/>
                <!-- <a-step title="定时策略" /> -->
            </a-steps>
          <!-- / Steps -->

        </div>
        <!-- / Header -->

        <!-- Wizard form cards -->
        <div class="mb-50">
          <!-- Step 1 : About && (folibRepository.type === 'hosted' || folibRepository.type === 'proxy' || folibRepository.type === 'group')-->
          <a-card
            v-if="step === 0"
            :bordered="false" class="header-solid">
            <div v-if="folibRepositoryEditDisabled">
              <h5 class="font-regular text-center">{{ $t('Storage.ClickNext') }} </h5>
                <p class="text-center">
                  {{ $t('Storage.WarehouseTypeRemain')}}
                </p>
            </div>
            <div v-if="folibRepositoryEditDisabled ? layoutChecked : layoutCheckedNow" class="text-desc">
              <h5 class="font-regular text-center">{{ layoutName }} </h5>
              <p class="text-center" style="text-align: left; text-indent:28px;">
                {{ $t(`Storage.${folibRepositoryEditDisabled ? layoutChecked : layoutCheckedNow}`) }}
              </p>
            </div>
            <div v-else>
                <h5 class="font-regular text-center">{{ folibRepositoryEditDisabled ? $t('Storage.ClickNext') : $t('Storage.HowToChoose') }} </h5>
                <p class="text-center">
                  {{ folibRepositoryEditDisabled ? $t('Storage.WarehouseTypeRemain') : $t('Storage.IdentifyWarehouseType') }}
                </p>
            </div>

            <a-form :form="form" class="mt-30" :hideRequiredMark="true">
              <a-row type="flex" :gutter="[24]">
                <a-col :span="24" :md="10" :lg="20" class="mx-auto">
                  <select-type @toggleCheckbox="toggleCheckbox" :layoutChecked="folibRepositoryEditDisabled ? layoutChecked : layoutCheckedNow" :isEdit="folibRepositoryEditDisabled" />
                  <a-checkbox-group class="d-none" v-model="checkedList" :options="checkboxOptions" />
                </a-col>
              </a-row>
              <a-row :gutter="[24]">
                <a-col :span="12">
                  <!--                  <a-button @click="moveStep(-1)" class="px-25">PREV</a-button>-->
                </a-col>
                <a-col :span="12" class="text-right">
                  <a-button :disabled="!layoutChecked" type="primary" @click="moveStep(1)" class="px-25">{{ $t('Storage.Next') }}
                  </a-button>
                </a-col>
              </a-row>
            </a-form>
          </a-card>

          <!-- Step 2 : Account  && (folibRepository.type === 'hosted' || folibRepository.type === 'proxy' || folibRepository.type === 'group')-->
          <a-card
            v-else-if="step === 1 && (folibRepository.type === 'hosted' || folibRepository.type === 'proxy' || folibRepository.type === 'group')"
            :bordered="false" class="header-solid">
            <h5 class="font-regular text-center">{{ $t('Storage.FillInInformation') }}</h5>
            <p class="text-center">
              {{ layoutChecked === 'docker' ? $t('Storage.DockerType') : $t('Storage.DifferentProcess') }}</p>
            <a-form :form="form" :hideRequiredMark="true">
              <a-row :gutter="[24]">
                <a-col :span="12">
                  <a-form-item class="mb-10" :label="$t('Storage.WarehouseName')" :colon="false">
                    <a-input :disabled="folibRepositoryEditDisabled" :placeholder="$t('Storage.KeywordPrompt')"
                      v-model="folibRepositoryIds" />
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" :label="$t('Storage.Strategy')" :colon="false">
                    <a-select :disabled="folibRepositoryEditDisabled"
                      default-value="hosted" v-model="folibRepository.type" @change="getRemote">
                      <a-select-option value="hosted">
                        {{ $t('Storage.Local') }}
                      </a-select-option>
                      <a-select-option value="proxy">
                        {{ $t('Storage.Agent') }}
                      </a-select-option>
                      <a-select-option value="group" v-if="this.layoutChecked !== 'cocoapods'">
                        {{ $t('Storage.Combination') }}
                      </a-select-option>
                    </a-select>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" :label="$t('Storage.VersioningStrategy')" :colon="false">
                    <a-select default-value="release" v-model="folibRepository.policy" :disabled="notEditPolicy">
                      <a-select-option value="release">
                        release
                      </a-select-option>
                      <a-select-option value="snapshot">
                        snapshot
                      </a-select-option>
                      <a-select-option value="mixed">
                        mixed
                      </a-select-option>
                    </a-select>
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item class="mb-10" :label="$t('Storage.RepositorySizeLimit')" :colon="false">
                    <a-input  :placeholder="$t('Storage.RepositorySizeLimit')" addon-after="GB" v-model="repositoryStorageMaxSize" />
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" :label="$t('Storage.ItemLimit')" :colon="false">
                    <a-input v-model="artifactMaxSize" addon-after="MB">
                    </a-input>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" :label="$t('Storage.ServiceStatus')" :colon="false">
                    <a-select default-value="In Service" v-model="folibRepository.status">
                      <a-select-option value="In Service">
                        {{ $t('Storage.OpenService') }}
                      </a-select-option>
                      <a-select-option value="Out of Service">
                        {{ $t('Storage.ShutdownService') }}
                      </a-select-option>
                    </a-select>
                  </a-form-item>
                </a-col>
              </a-row>
              <a-row :gutter="[24]">
                  <a-col :span="12">
                      <a-form-item class="mb-10" :label="$t('Storage.StorageThreshold')" :colon="false">
                          <a-input-number style="width: 100%"
                              :default-value="100"
                              :min="0"
                              :max="100"
                              :formatter="value => `${value}%`"
                              :parser="value => value.replace('%', '')"
                              v-model="repositoryStorageThreshold"
                          />
                      </a-form-item>
                  </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" :label="$t('Storage.EnableCustomLayout')" :colon="false">
                    <a-switch v-model="folibRepository.enableCustomLayout" @change="enableCustomLayoutChange"></a-switch>
                  </a-form-item>
                </a-col>
                <a-col :span="6" v-if="folibRepository.enableCustomLayout">
                  <a-form-item class="mb-10" :label="$t('Storage.CustomLayout')" :colon="false">
                    <a-select v-model="folibRepository.customLayout" style="width: 100%" model="default" show-search allowClear
                      :dropdown-style="{ maxHeight: '240px', overflow: 'auto' }"
                      :filter-option="true"
                      :placeholder="$t('Storage.CustomLayoutTip')">
                      <a-select-option v-for="(layout, index) in customLayoutList" :key="index" :value="layout.artifactPathPattern">
                        {{ layout.layoutName }}
                      </a-select-option>
                    </a-select>
                  </a-form-item>
                </a-col>
<!--                <a-col :span="6">-->
<!--                    <a-form-item class="mb-10" :label="$t('Storage.StorageThreshold')" :colon="false">-->
<!--                        <a-input  addon-after="%" v-model="repositoryStorageThreshold" />-->
<!--                    </a-form-item>-->
<!--                </a-col>-->
              </a-row>
              <a-row :gutter="[24]">
                <a-col :span="6">
                  <a-form-item class="mb-10" label="" :colon="false">
                    <a-checkbox v-model="folibRepository.trashEnabled">
                      {{$t('Storage.On')}}{{$t('Storage.RecycleBin')}}
                      <!-- {{ folibRepository.trashEnabled ? $t('Storage.On')  : $t('Storage.Off') }} -->
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" label="" :colon="false">
                    <a-checkbox v-model="folibRepository.allowsDeletion">
                      {{ $t('Storage.Allowed') }}{{ $t('Storage.Delete') }}
                      <!-- {{ folibRepository.allowsDeletion ? $t('Storage.Allowed') : $t('Storage.NotAllowed') }} -->
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" label="" :colon="false">
                    <a-checkbox v-model="folibRepository.allowsForceDeletion">
                      {{ $t('Storage.Allowed') }}{{ $t('Storage.ForcedDeletion') }}
                      <!-- {{ folibRepository.allowsForceDeletion ? $t('Storage.Allowed') : $t('Storage.NotAllowed') }} -->
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" label="" :colon="false">
                    <a-checkbox v-model="folibRepository.allowsDeployment">
                      {{ $t('Storage.Allowed') }}{{ $t('Storage.UploadDeploy') }}
                      <!-- {{ folibRepository.allowsDeployment ? $t('Storage.Allowed') : $t('Storage.NotAllowed') }} -->
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" label="" :colon="false">
                    <a-checkbox v-model="folibRepository.allowsRedeployment">
                      {{ $t('Storage.Allowed') }}{{ $t('Storage.UploadOverlay') }}
                      <!-- {{ folibRepository.allowsRedeployment ? $t('Storage.Allowed') : $t('Storage.NotAllowed') }} -->
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" label="" :colon="false">
                    <a-checkbox v-model="folibRepository.allowsDirectoryBrowsing">
                      {{ $t('Storage.Allowed') }}{{ $t('Storage.DirectoryBrowsing') }} 
                      <!-- {{ folibRepository.allowsDirectoryBrowsing ? $t('Storage.Allowed') : $t('Storage.NotAllowed') }} -->
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" label="" :colon="false">
                    <a-checkbox v-model="folibRepository.syncEnabled">
                      {{ $t('Storage.On') }}{{ $t('Storage.SyncRepository') }}
                      <!-- {{ folibRepository.syncEnabled ?  $t('Storage.On') : $t('Storage.Off')  }} -->
                    </a-checkbox>
                  </a-form-item>
                </a-col>
              </a-row>
              <a-row :gutter="[24]">
                <a-col :span="18">
                  <a-button @click="moveStep(-1)" class="px-25">{{ $t('Storage.Back') }}</a-button>
                </a-col>
                <a-col :span="3" class="text-right">
                    <a-button  v-if="folibRepository.type === 'hosted'" @click="addOrUpdateRepositorySecond(true,false)" class="px-25">
                        {{ $t('Storage.Save') }}
                    </a-button>
                </a-col>
                  <a-col :span="3" class="text-right">
                      <a-button v-if="folibRepository.type === 'hosted'" type="primary"
                                @click="addOrUpdateRepositorySecond(true)" class="px-25">
                          {{ $t('Storage.Next') }}
                      </a-button>

                      <!-- <a-button v-if="folibRepository.type === 'hosted'" style="margin-left: 20px"
                        @click="addOrUpdateRepositorySecond(true)" class="px-25">
                        {{ folibRepositoryEditDisabled? '修改': '创建' }}并设置定时策略</a-button> -->

                      <a-button v-else-if="folibRepository.type !== 'hosted'" type="primary" @click="moveStep(1)"
                                class="px-25">{{ $t('Storage.Next') }}
                      </a-button>
                  </a-col>
              </a-row>
            </a-form>
          </a-card>

          <!-- Step 3 : Address -->
            <!--  -->
          <a-card v-else-if="step === 2 && (folibRepository.type === 'proxy')" :bordered="false" class="header-solid">
            <h5 class="font-regular text-center">{{ $t('Storage.WarehouseConfig') }}</h5>
            <p class="text-center">
              {{ $t('Storage.RemoteLibraryAddress') }}</p
            <a-form :form="form" :hideRequiredMark="true">
              <a-row :gutter="[24]">
                <a-col :span="12">
                  <a-form-item class="mb-10" :label="$t('Storage.RemoteAccessAddress')" :colon="false">
                    <a-input :placeholder="$t('Storage.AddressFormat')" v-model="folibRepository.remoteRepository.url" />
                  </a-form-item>
                </a-col>
                <a-col :span="2">
                  <a-form-item class="mb-10" label=" "  :colon="false">
                    <a-button @click="verifyAlive()">{{ $t('Storage.Test') }}</a-button>
                  </a-form-item>
                </a-col>
                <a-col :span="5">
                  <a-form-item class="mb-10" :label="$t('Storage.Username')" :colon="false">
                    <a-input v-model="folibRepository.remoteRepository.username" autocomplete="new-text"
                      :placeholder="$t('Storage.AccessUser')" />
                  </a-form-item>
                </a-col>
                <a-col :span="5">
                  <a-form-item class="mb-10" :label="$t('Storage.Password')" :colon="false">
                    <a-input-password v-model="folibRepository.remoteRepository.password" autocomplete="new-password"
                      :placeholder="$t('Storage.AccessPassword')" />
                  </a-form-item>
                </a-col>

                <!--git  layoutChecked === 'go' -->
                <div v-if="layoutChecked === 'go' ">

                  <div v-if="folibRepository.repositoryConfiguration">
                      <div v-for="(item,index) in folibRepository.repositoryConfiguration.gitVCS" :key="index">
                        <a-col :span="12">
                          <a-form-item class="mb-10" :label="$t('Storage.GitProviderCredential')" :colon="false">
                            <a-input :placeholder="$t('Storage.AddressFormat')"
                                     v-model="item.url"/>
                          </a-form-item>
                        </a-col>

                        <a-col :span="5">
                          <a-form-item class="mb-10" :label="$t('Storage.GitUsername')" :colon="false">
                            <a-input v-model="item.username" autocomplete="new-text"
                                     :placeholder="$t('Storage.AccessUser')"/>
                          </a-form-item>
                        </a-col>
                        <a-col :span="5">
                          <a-form-item class="mb-10" :label="$t('Storage.GitPassword')" :colon="false">
                            <a-input-password v-model="item.password"
                                              autocomplete="new-password"
                                              :placeholder="$t('Storage.AccessPassword')"/>
                          </a-form-item>
                        </a-col>
                        <a-col :span="2">
                          <a-form-item class="mb-10" label=" " :colon="false">
                            <a-button @click="delGitItem(index)">{{ $t('Storage.Delete') }}</a-button>
                          </a-form-item>
                        </a-col>
                      </div>
                    </div>
                    <a-col :span="24">
                        <a-button @click="addGitItem()">{{ $t('Storage.AddGitCredential') }}</a-button>
                    </a-col>

                </div>
                <a-col :span="12">
                  <a-form-item class="mb-10" :label="$t('Storage.TimedCheck')" :colon="false">
                    <a-input :placeholder="$t('Storage.DefaultTime')" v-model="folibRepository.remoteRepository.checkIntervalSeconds"
                      addon-after="s" />
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item class="mb-10" :label="$t('Storage.InspectionMechanism')" :colon="false">
                    <a-select default-value="None" style="width:100%;text-align: start;vertical-align: top;" v-model="folibRepository.remoteRepository.checksumPolicy">
                      <a-select-option value="None">
                        {{ $t('Storage.None') }}
                      </a-select-option>
                      <a-select-option value="Strict">
                        {{ $t('Storage.Strict') }}
                      </a-select-option>
                      <a-select-option value="Log">
                        {{ $t('Storage.LogRecording') }}
                      </a-select-option>
                      <a-select-option value="Warn">
                        {{ $t('Storage.Advanced') }}
                      </a-select-option>
                    </a-select>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" label="" :colon="false">
                    <a-checkbox v-model="enableHostProxy" @change="proxyConfigurationHandle">
                      {{ $t('Storage.On') }}{{ $t('Storage.LocalAgent') }}
                      <!-- {{ enableHostProxy ? $t('Storage.On') : $t('Storage.Off') }} -->
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" label="" :colon="false">
                    <a-checkbox v-model="folibRepository.remoteRepository.allowsDirectoryBrowsing">
                      {{ $t('Storage.Allowed') }}{{ $t('Storage.DirectoryBrowsing') }}
                      <!-- {{ folibRepository.remoteRepository.allowsDirectoryBrowsing ? $t('Storage.Allowed') : $t('Storage.NotAllowed') }} -->
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" label="" :colon="false">
                    <a-checkbox v-model="folibRepository.remoteRepository.autoBlocking">
                      {{ $t('Storage.On') }}{{$t('Storage.AutomaticLock')}}
                      <!-- {{ folibRepository.remoteRepository.autoBlocking ? $t('Storage.On') : $t('Storage.Off') }} -->
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" label="" :colon="false">
                    <a-checkbox v-model="folibRepository.remoteRepository.checksumValidation">
                      {{$t('Storage.On')}}{{ $t('Storage.ChecksumCheck') }}
                      <!-- {{ folibRepository.remoteRepository.checksumValidation ? $t('Storage.On') : $t('Storage.Off') }} -->
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" label="" :colon="false">
                    <a-checkbox v-model="folibRepository.remoteRepository.downloadRemoteIndexes">
                      {{ $t('Storage.Download') }}{{ $t('Storage.RemoteIndexDownload') }}
                      <!-- {{ folibRepository.remoteRepository.downloadRemoteIndexes ? $t('Storage.Download') : $t('Storage.NoDownload') }} -->
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" label="" :colon="false">
                    <a-checkbox v-model="folibRepository.syncEnabled">
                      {{ $t('Storage.On') }}{{ $t('Storage.SyncRepository') }}
                      <!-- {{ folibRepository.syncEnabled ?  $t('Storage.On') : $t('Storage.Off')  }} -->
                    </a-checkbox>
                  </a-form-item>
                </a-col>
              </a-row>

              <a-row v-if="enableHostProxy" :gutter="[24]">
                <a-col :span="24">
                  <h5 class="font-regular text-center">{{ $t('Storage.ProxySetup') }}</h5>
                  <p class="text-center">{{ $t('Storage.UseProxySettings') }}</p>
                </a-col>
                <a-col :span="8">
                  <a-form-item class="mb-10" label="ProxyHost" :colon="false">
                    <a-input placeholder="ProxyHost" v-model="folibRepository.proxyConfiguration.host" />
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" :label="$t('Storage.Port')" :colon="false">
                    <a-input v-model="folibRepository.proxyConfiguration.port" :placeholder="$t('Storage.Port')" />
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" :label="$t('Storage.Protocol')" :colon="false">
                    <a-select default-value="None" :allowClear="true" v-model="folibRepository.proxyConfiguration.type" :placeholder="$t('Storage.Protocol')">
                      <a-select-option value="HTTP">
                        HTTP
                      </a-select-option>
                      <a-select-option value="HTTPS">
                        HTTPS
                      </a-select-option>
                    </a-select>
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" :label="$t('Storage.Username')" :colon="false">
                    <a-input v-model="folibRepository.proxyConfiguration.username" :placeholder="$t('Storage.NoUserName')" />
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" :label="$t('Storage.Password')" :colon="false">
                    <a-input-password v-model="folibRepository.proxyConfiguration.password" :placeholder="$t('Storage.NoPassword')" />
                  </a-form-item>
                </a-col>

              </a-row>
              <a-row :gutter="[24]">
                <a-col :span="12">
                  <a-button @click="moveStep(-1)" class="px-25">{{ $t('Storage.Back') }}</a-button>
                </a-col>
                <a-col :span="9" class="text-right">
                      <a-button  v-if="folibRepository.type === 'proxy'" @click="addOrUpdateRepositorySecond(true,false)" class="px-25">
                          {{ $t('Storage.Save') }}
                      </a-button>
               </a-col>
                <a-col :span="3" class="text-right">
                  <a-button type="primary" @click="addOrUpdateRepositoryHandel(true)" class="px-25">
                      {{ $t('Storage.Next') }}
                  </a-button>
                  <!-- <a-button style="margin-left: 20px" @click="addOrUpdateRepositoryHandel(true)" class="px-25">
                    {{ folibRepositoryEditDisabled? '修改': '创建' }}并设置定时策略</a-button> -->
                </a-col>
              </a-row>
            </a-form>
          </a-card>

          <a-card v-else-if="step === 2 && (folibRepository.type === 'group')" :bordered="false" class="header-solid">
            <h5 class="font-regular text-center">{{ $t('Storage.CompositeWarehouse') }}</h5>
            <p class="text-center">{{ $t('Storage.WarehousePortfolio') }}</p>
            <div class="kanban-page mb-24">
              <div id="kanban" class="kanban">
                <draggable :list="i18nBoards" :animation="200" class="kanban-boards" ghost-class="ghost-card"
                  group="i18nBoards">
                  <FolibKanbanBoard  v-for="(board) in i18nBoards" :key="board.id" :board="board">
                    <draggable :list="board.tasks" :animation="200" ghost-class="ghost-card" group="tasks" ref="draggable" :style="{height: draggableHeight, overflowY: 'auto'}">
                      <FolibKanbanTask  ref="kanbanBoard" v-for="(task) in board.tasks" :key="task.id" :task="task" :boardId="board.id" @setDefault="setDefaultRepository" @cancelDefault="cancelDefaultRepository">
                      </FolibKanbanTask>

                    </draggable>

                  </FolibKanbanBoard>
                  <!-- / Kanban Board -->

                </draggable>
              </div>
            </div>
            <a-row :gutter="[24]">
              <a-col :span="12">
                <a-button @click="moveStep(-1)" class="px-25">{{ $t('Storage.Back') }}</a-button>
              </a-col>
                <a-col :span="9" class="text-right">
                    <a-button  v-if="folibRepository.type === 'group'" @click="addOrUpdateRepositorySecond(true,false)" class="px-25">
                        {{ $t('Storage.Save') }}
                    </a-button>
                </a-col>
              <a-col :span="3" class="text-right">
                <a-button type="primary" @click="addOrUpdateRepositoryHandel(true)" class="px-25">
                    {{ $t('Storage.Next') }}
                </a-button>
                <!-- <a-button style="margin-left:20px" @click="addOrUpdateRepositoryHandel(true)" class="px-25">
                  {{ folibRepositoryEditDisabled? '修改': '创建' }}并设置定时策略</a-button> -->
              </a-col>
            </a-row>
          </a-card>
<!--          <a-card v-else-if="step === 3" :bordered="false" class="header-solid">-->
<!--            <h5 class="font-regular text-center">{{ $t('Storage.CustomizedStrategy') }}</h5>-->
<!--            <p class="text-center">{{ $t('Storage.TimingStrategy') }}</p>-->
<!--            <a-form :form="form" :hideRequiredMark="true">-->

<!--              <div v-for="(i, index) in cronCanSetList" :key="index">-->
<!--                <a-row type="flex" align="middle">-->
<!--                  <a-col style="min-width: 40px;" class="text-center">-->
<!--                    <a-icon type="clock-circle" class="text-gray-6" style="font-size: 18px;" />-->
<!--                  </a-col>-->
<!--                  <a-col class="pl-15">-->
<!--                    <p class="mb-0">{{ i.name }}</p>-->
<!--                    <small class="text-dark">{{ i.description }}</small>-->
<!--                  </a-col>-->
<!--                  <a-col :span="24" :md="12" class="ml-auto"-->
<!--                    style="display: flex; align-items: center; justify-content: flex-end">-->
<!--                    <a-tag v-if="i.isSetted && i.isSetted.uuid" color="success" class="ant-tag-success font-bold">{{ $t('Storage.HaveSet') }}-->
<!--                    </a-tag>-->
<!--                    <span class="ml-5">{{ i.scope }}</span>-->
<!--                    <a-button @click="cronShowHandle(i, index)" type="link" class="btn-more ml-5">-->
<!--                      {{ $t('Storage.ExpandSetting') }}-->
<!--                      <a-icon :type="i.isShow ? 'arrow-down' : 'arrow-right'" />-->
<!--                    </a-button>-->
<!--                  </a-col>-->
<!--                </a-row>-->
<!--                <a-card v-if="i.isShow" :bordered="false" class="bg-gray-3 shadow-0 mb-24"-->
<!--                  :bodyStyle="{ padding: '8px' }">-->
<!--                  <a-row type="flex" align="middle">-->
<!--                    <a-col>-->
<!--                      <p class="font-semibold mb-0 ml-10">{{ i.isSetted.jobClass }}</p>-->
<!--                    </a-col>-->
<!--                    <a-col class="ml-auto">-->
<!--                      <a-input v-model="i.isSetted.cronExpression" size="small" class="font-regular text-sm text-dark"-->
<!--                        style="width: 100px;" />-->
<!--                    </a-col>-->
<!--                    <a-col class="ml-auto">-->
<!--                      <span class="mr-15">{{ i.isSetted.oneTimeExecution ? $t('Storage.ExecuteOnce') : $t('Storage.LoopExecution') }}</span>-->
<!--                      <a-switch v-model="i.isSetted.oneTimeExecution"-->
<!--                        @change="oneTimeExecutionChange($event, i.isSetted)" />-->
<!--                    </a-col>-->
<!--                    <a-col class="ml-auto">-->
<!--                      <span class="mr-15">{{ i.isSetted.immediateExecution ? $t('Storage.ImmediateExecution') : $t('Storage.NoImmediateExecution') }}</span>-->
<!--                      <a-switch v-model="i.isSetted.immediateExecution"-->
<!--                        @change="immediateExecutionChange($event, i.isSetted)" />-->
<!--                    </a-col>-->
<!--                  </a-row>-->
<!--                  <hr v-if="i.fields.length > 2" class="gradient-line my-10">-->
<!--                  <a-row type="flex" align="middle">-->
<!--                    <a-col v-if="i.fields.length > 2" style="margin-right: 15px">-->
<!--                      <p class="font-semibold mb-0 ml-10">{{ $t('Storage.OtherParameters') }}:</p>-->
<!--                    </a-col>-->
<!--                    <div v-if="i.fields.length > 2">-->
<!--                      <div v-for="(f, index) in i.fields" :key="index">-->
<!--                        <a-col v-if="f.name !== 'storageId' && f.name !== 'repositoryId'" class="ml-auto">-->
<!--                          <span style="margin-left: 15px" class="mr-15">{{ f.name }}</span>-->
<!--                          <a-input v-if="f.type === 'string'" v-model="f.value" size="small"-->
<!--                            class="font-regular text-sm text-dark" style="width: 250px;" />-->
<!--                          <a-input-number v-if="f.type === 'int' && f.name === 'numberToKeep'" v-model="f.value"-->
<!--                            size="small" class="font-regular text-sm text-dark" style="width: 120px;" />-->
<!--                          <a-input-number :min="1" v-if="f.type === 'int' && f.name === 'storageDay'" v-model="f.value"-->
<!--                                          size="small" class="font-regular text-sm text-dark" style="width: 120px;" />-->
<!--                          <a-date-picker v-if="f.type === 'int' && f.name === 'keepPeriod'" v-model="f.value"-->
<!--                            size="small" class="font-regular text-sm text-dark" style="width: 120px;" />-->
<!--                          <a-switch v-if="f.type === 'boolean'" v-model="f.value" @change="() => { $forceUpdate() }" />-->
<!--                        </a-col>-->
<!--                      </div>-->
<!--                    </div>-->
<!--                  </a-row>-->
<!--                  <a-row :gutter="[24]">-->
<!--                    <a-col :span="12">-->
<!--                    </a-col>-->
<!--                    <a-col :span="12" class="text-right">-->
<!--                      <a-button @click="saveCronOneSetHandle(i)" type="primary" size="small" shape="circle"-->
<!--                        icon="save" />-->
<!--                      <a-button v-if="i.isSetted.uuid" @click="delCronOneSetHandle(i)" style="margin-left: 15px"-->
<!--                        type="danger" size="small" shape="circle" icon="delete" />-->
<!--                    </a-col>-->
<!--                  </a-row>-->
<!--                </a-card>-->
<!--                <hr class="gradient-line my-10">-->
<!--              </div>-->
<!--              <hr class="gradient-line my-10">-->
<!--              <a-row :gutter="[24]">-->
<!--                <a-col :span="12">-->
<!--                </a-col>-->
<!--                <a-col :span="12" class="text-right">-->
<!--                  <a-button type="primary" @click="andCronSetHandle" class="px-25">{{ $t('Storage.CompleteSetting') }}</a-button>-->
<!--                </a-col>-->
<!--              </a-row>-->
<!--            </a-form>-->
<!--          </a-card>-->

           <a-card v-else-if="(step === 2 && folibRepository.type === 'hosted') ||
            (  folibRepository.type === 'proxy' && step === 3) ||
            (step === 3 && folibRepository.type === 'group')" :bordered="false" class="header-solid">
               <a-row>
                   <a-col :span="24">
                       <Permission ref="permission" :isShow="isShow" :folibRepository="this.folibRepositoryData" :settingVisible="settingVisible" @settingDrawerClose="settingDrawerClose"></Permission>
                   </a-col>
               </a-row>
               <a-row>
                   <a-col :span="12">
                       <a-button @click="moveStep(-1)" class="px-25">{{ $t('Storage.Back') }}</a-button>
                   </a-col>
                   <a-col :span="12" style="text-align: right;">
                       <a-button type="primary" @click="doPermission()" class="px-25">{{ $t('Storage.Next') }}</a-button>
                   </a-col>
               </a-row>
           </a-card>
            <a-card v-else-if="(step === 3 && folibRepository.type === 'hosted') ||
            (  folibRepository.type === 'proxy' && step === 4) ||
            (step === 4 && folibRepository.type === 'group')" :bordered="false" class="header-solid">

                <a-row>
                    <a-col :span="12">
                        <CronTask :folibRepository="this.folibRepositoryData" @settingDrawerClose="settingDrawerClose"></CronTask>
                    </a-col>
                </a-row>
                <a-row>
                    <a-col :span="12">
                        <a-button @click="moveStep(-1)" class="px-25">{{ $t('Storage.Back') }}</a-button>
                    </a-col>
                    <a-col :span="12" style="text-align: right;">
                        <a-button type="primary" @click="moveStep(1)" class="px-25">{{ $t('Storage.Next') }}</a-button>
                    </a-col>
                </a-row>
            </a-card>
            <a-card v-else-if="(step === 4 && folibRepository.type === 'hosted') ||
            (  folibRepository.type === 'proxy' && step === 5) ||
            (step === 5 && folibRepository.type === 'group')" :bordered="false" class="header-solid">
                <a-row>
                    <a-col :span="24">
                        <UnionRepository ref="unionRepository" :isShow="isShow" :folibRepository="this.folibRepositoryData" :settingVisible="settingVisible" @settingDrawerClose="settingDrawerClose"></UnionRepository>
                    </a-col>
                </a-row>
                <a-row>
                    <a-col :span="12">
                        <a-button @click="moveStep(-1)" class="px-25">{{ $t('Storage.Back') }}</a-button>
                    </a-col>
                    <a-col :span="12" style="text-align: right;">
                        <a-button type="primary" @click="doUnionRepository" class="px-25">{{ $t('Storage.Next') }}</a-button>
                    </a-col>
                </a-row>



            </a-card>
            <a-card v-else-if="(step === 5 && folibRepository.type === 'hosted') ||
            (  folibRepository.type === 'proxy' && step === 6) ||
            (step === 6 && folibRepository.type === 'group')" :bordered="false" class="header-solid">
                <a-row>
                    <a-col :span="24">
                        <Scan ref="scan" :isShow="isShow" :folibRepository="this.folibRepositoryData" :settingVisible="settingVisible" @settingDrawerClose="settingDrawerClose"></Scan>
                    </a-col>
                </a-row>

                <a-row>
                    <a-col :span="12">
                        <a-button @click="moveStep(-1)" class="px-25">{{ $t('Storage.Back') }}</a-button>
                    </a-col>
                    <a-col :span="12" style="text-align: right;">
                        <a-button type="primary" @click="doScan()" class="px-25"> {{ $t('Storage.Complete') }}{{ folibRepositoryEditDisabled ? $t('Storage.Edit') : $t('Storage.create') }}</a-button>
                    </a-col>
                </a-row>
            </a-card>
        </div>
      </div>
    </a-drawer>
  </div>

</template>

<script>
import {
    getStorages,
    updateStorages,
    getLibrary,
    getLibraryFilter,
    getLibraryByQuery,
    addOrUpdateRepository,
    getRepositoryResponseEntity,
    delRepositoryResponseEntity,
    getBaseUrl,
    createStorages,
    deleteStorages,
    crontasksList,
    crontasksByRepository,
    creatCronOne,
    updateCronOne,
    delCronOne,
    getStoragesAndRepositories,
    aliveRepository, repositoryEnableUsers,
    queryRepositoriesByStorage
} from "@/api/folib"
import {
  queryCustomLayoutList,
} from "@/api/customLayout"
import { getUsers, queryUser } from "@/api/users"
import CardProjectFolib from "@/components/Cards/CardProjectFolib"
import { getLayoutType, getLayoutRepoPrefix, genLayoutType, groupRepositoriesBuild, objectToGroupRepositories } from "@/utils/layoutUtil"
import draggable from "vuedraggable"
import FolibKanbanBoard from "@/components/Kanban/FolibKanbanBoard"
import FolibKanbanTask from "@/components/Kanban/FolibKanbanTask"
import storage from 'store'
import store from '@/store'
import { checkMachineCode } from "@/api/settings"
import { hasRole, isAdmin, hasPermission, isLogin } from "@/utils/permission"
import language from "@/store/modules/language";
import Overview from "../StorageMonitoring/components/Overview"
import StorageInfo from "../StorageMonitoring/components/StorageInfo"
import storageInfoCard from './Storage-components/storage-info-card.vue'
import LibView from './LibView.vue'
import CronTask from "@/views/Storage/components/Cron/index.vue";
import UnionRepository from "@/views/Storage/components/UnionRepository/index.vue";
import Scan from "@/views/Storage/components/Scan/index.vue";
import Permission from "@/views/Storage/components/Permission/index.vue";
import selectType from './Storage-components/select-type.vue'
import repositoryTree from './Storage-components/repository-tree.vue'
import storageList from './Storage-components/storage-list.vue'

export default {
  inject: ["reload"],
  components: {
      Permission,
      Scan,
      UnionRepository,
      CronTask,
    CardProjectFolib,
    draggable,
    FolibKanbanBoard,
    FolibKanbanTask,
    Overview,
    StorageInfo,
    storageInfoCard, // 存储空间卡片信息组件
    LibView,
    selectType,
    repositoryTree,
    storageList
  },
  props: {
    navbarFixed: {
            type: Boolean,
            default: true,
        },
    anonymous: {
            type: Boolean,
            default: false,
        },
  },
  data() {
    const checkStorageId = (rule, value, callback) => {
      if (value) {
        var reg = /^[a-zA-Z0-9_.\\-]+$/
        if (reg.test(value) === false) {
          callback(new Error(this.$t('Storage.SpaceNameLimit')))
        } else if(value.length < 1 || value.length > 30) {
          callback(new Error(this.$t('Storage.SpaceNameLengthLimit')))
        } else {
          callback()
        }
      } else if (!value) {
        callback(new Error(this.$t('Storage.EnterSpaceName')))
      } else {
        callback()
      }
    }
    return {
      userInfo: {},
      showStorageUpdate: false,
      userList: [],
      baseUrl: null,
      folibVisible: false,
      storageData: [],
      cronCanSetList: [],
      cronSettedList: [],
      customStorage: false,
      storagePrefix: null,
      currentStorage: {
        id: null,
        basedir: null,
        admin: undefined,
        users: [],
        storageProvider: 'local',
        storageMaxSize: 0,
        bucket: null,
        syncEnabled: false
      },
      currentDefultStorage: {
        id: null,
        basedir: null,
        admin: undefined,
        users: [],
        storageProvider: 'local',
        storageMaxSize: 0,
        bucket: null,
        syncEnabled: false
      },
      showsTorageFormModal: false,
      delForm: this.$form.createForm(this, { name: "del" }),
      storageCreateData: {
        id: null,
        basedir: null,
        admin: undefined,
        storageProvider: 'local',
        storageMaxSize: 0,
        bucket: null,
        users: [],
        syncEnabled: false
      },
      storageCreateDefultData: {
        id: null,
        basedir: null,
        admin: undefined,
        storageProvider: 'local',
        storageMaxSize: 0,
        bucket: null,
        users: [],
        syncEnabled: false
      },
      visibility: true,
      slack: true,
      spotify: true,
      atlassian: true,
      asana: false,
      tags: ['Vuejs', 'Angular', 'React'],
      repositories: [],
      queryData: {
        term: null,
        storageId: null,
        withStorageId: true,
        type: null,
        layout: null
      },
      //抽屉相关
      layoutChecked: null,
      layoutCheckedNow:null,
      layoutName:null,
      enableHostProxy: false,
      step: 0,
      // Checkbox'es array of checked options.
      checkedList: ['Design'],
      // Checkbox'es array of all options.
      checkboxOptions: ['Design', 'Code', 'Develop'],
      willDelId: null,
      deleteFormVisible: false,
      deleteBtnVisible: false,
      forceDeleteBtnVisible: false,
      // Step's form object
      form: this.$form.createForm(this, { name: 'steps' }),
      folibRepositoryIds: "",
      artifactMaxSize: 100,
      storageMaxSize: 0,
      folibRepositoryEditDisabled: false,
      isShow:false,
      folibRepositoryData:{
          id: "",
          storageId:"",
          layout: "",
      },
      repositoryStorageMaxSize: 0,
      repositoryStorageThreshold:0,
      folibRepository: {
        allowsDeletion: true,
        allowsDeployment: true,
        allowsDirectoryBrowsing: true,
        allowsForceDeletion: false,
        allowsRedeployment: false,
        artifactCoordinateValidators: null,
        artifactMaxSize: 100,
        basedir: null,
        storageMaxSize: 0,
        storageThreshold:0,
        checksumHeadersEnabled: true,
        groupRepositories: [],
        httpConnectionPool: null,
        id: "",
        layout: "",
        subLayout: "",
        policy: "release",
        proxyConfiguration: {
          host: null,
          nonProxyHosts: [],
          password: null,
          port: null,
          type: null,
          username: null
        },
        remoteRepository: {
          allowsDirectoryBrowsing: true,
          autoBlocking: true,
          autoImportRemoteSSLCertificate: false,
          checkIntervalSeconds: 60,
          checksumPolicy: 'None',
          checksumValidation: true,
          downloadRemoteIndexes: true,
          password: "",
          url: "",
          username: ""
        },
        repositoryConfiguration: null,
        secured: false,
        status: "In Service",
        storageProvider: "local",
        trashEnabled: true,
        type: "hosted",
        syncEnabled: false,
      },
      isTrashView: false,
      boards: [
        {
          id: "folibHub",
          title: "可选择制品仓库",
          i18nKey: 'Storage.SelectableRepository',
          tasks: []
        },
        {
          id: "folibGoup",
          title: "已组合仓库",
          i18nKey: 'Storage.CompositeRepository',
          tasks: []
        }
      ],
      storageRules: {
        id: [
          { required: true, trigger: 'blur', validator: checkStorageId }
        ]
      },
      draggableHeight: '100vh',
      goRepositoryConfiguration:{
        gitVCS:[

        ],
        gitVCS2:[]
        //there may be other VCS ...
      },
      queryParams:{
        groupId: null,
        storageId: null,
        layout: null,
        type: null,
        page:1,
        limit: 10,
        total:0,
      },
      layoutType:'isFilter',
      isChecked: false,
      isShowOverview: false,
     permissionForm: {
        allowAnonymous: true,
        scope: 1,
        userList: []
     },
     settingVisible: false,
     stepsStatus:"process",
     isRepoExist:true,
     notEditPolicy:false,
      userQueryParams: {
        page: 1,
        limit: 50,
        total: 0,
        matchUsername: undefined,
      },
      libViewKey:0,
      groupDefaultRepository:undefined,
      customLayoutList: [],
      switchDisabled:true,
    };
  },
  watch: {
    isChecked(val){
      this.getDetailInfo(val)
    },
    '$i18n.locale'() {
      this.$forceUpdate();
    },
    currentStorage:{
      handler(val){
        console.log(val);

      },
      deep:true
    },
      layoutChecked(newVal, oldVal) {
          console.log('Selected value changed from', oldVal, 'to', newVal);
          console.log(newVal !=="maven");
          // 在这里处理值改变的逻辑
          if(newVal !=="maven"){
              this.folibRepository.policy="mixed"
              this.notEditPolicy=true;
          }else {
              this.notEditPolicy=false;
          }
          console.log("notEditPolicy",this.notEditPolicy);

      }
  },
  async created() {
    this.userInfo = store.state.user
    await  this.getStorages();
    await  this.getBaseUrl();

    const params = storage.get('libView_repository')

    if (params) {
      this.currentStorage.id = params.item.storageId
      this.queryParams.storageId = params.item.storageId
    }

    if (!this.currentStorage.id && this.storageData && this.storageData.length > 0) {
      this.currentStorage.id = this.storageData[0].id
    }
    this.getStorage(this.currentStorage.id)
    this.queryCustomLayoutList()
  },
  computed: {
    // isChecked(){
    //   return this.$store.state.isChecked
    // },
    isLogin() {
      return isLogin()
    },
    i18nBoards() {
      return this.boards.map(column => {
        if (column.i18nKey) {
          column.title = this.$t(column.i18nKey);
        }
        return column;
      });
    },
    language() {
      return this.$store.state.language.lang
    },
  },
  mounted(){
    this.$store.commit('setNewDetailPage',true)
    this.$store.commit('setNewDetailPage',false)
    // this.$store.commit('setSwitchDisabled',true)
    this.switchDisabled = true
  },
  methods: {
    // 制品仓库页面专属。用于切换仓库和存储空间的展示模式
    checkMode(key){
      if(this.switchDisabled){
        return
      }
      setTimeout(() => {
        this.isChecked = key
        // this.$store.commit('setIsChecked',key)
      }, 0);
    },
    getRemote(val){
      if(val == 'proxy'){
          this.folibRepository.remoteRepository = {
            allowsDirectoryBrowsing: true,
            autoBlocking: true,
            autoImportRemoteSSLCertificate: false,
            checkIntervalSeconds: 60,
            checksumPolicy: 'None',
            checksumValidation: true,
            downloadRemoteIndexes: true,
            password: "",
            url: "",
            username: ""
          }
      }
    },
    // 展示存储概览
    showOverview(val){
      this.isShowOverview = val == 2
    },
    changeMoudles(){
      // this.isChecked = !this.isChecked
      this.$store.commit('setIsChecked',!this.isChecked)
      if(this.isChecked){
        this.$refs.libview.myMounted()
      }
    },
    getDetailInfo(val,type){
      console.log(val,type)
      if(this.repositories.length){
        const item = this.repositories[0]
        storage.set("libView_repository", { item, baseUrl: this.baseUrl })
        this.repositories = []
      }
      this.$nextTick(() => {
        if(val){
          this.$refs.repositoryTree.loadingMoreShow(true)
          // this.$refs.libview.myMounted()
          this.queryParams.limit = 20 // 20
        }else{
          this.$store.commit('setNewDetailPage',false) // 将newDetailPage参数复原，以免影响到原有模式的详情页面
          this.queryParams.limit = 10
          // 清空搜索仓库模式下的搜索条件，防止下次进入时影响数据
          this.$refs.storageInfoCard.emptyQuery()
        }
        // 每次切换模式 都清空除storageId以外的当前已存在的搜索条件
        this.queryParams.page = 1
        this.queryParams.layout = null
        this.queryParams.type = null
        const params = {
          storageId: this.currentStorage.id,
          layout: null,
          type: null,
          limit: this.queryParams.limit,
          page: this.queryParams.page
        }
        // 获取切换模式后第一次加载的数据
        this.getQueryStorage(params,type)
      })
    },
    // 右键菜单选择操作
    handleMenuClickTree(active,currentTreeNode){
      this.$nextTick(() => {
          this.$refs.libview.handleMenuClickTree(active,currentTreeNode)
      })
    },
    reloadTree(){
      this.$refs.repositoryTree.reload(false)
    },
    // 点击仓库
    repositorySelect(item){
      storage.set("libView_repository", { item, baseUrl: this.baseUrl })
      this.$nextTick(() => {
        this.libViewKey ++
        this.$refs.libview.myMounted()
      })
    },  
    // 点击仓库下的文件,item为当前点击节点的最顶层父节点（仓库）
    treeSelect(key, e, item) {
      this.libViewKey ++
      storage.set("libView_repository", { item, baseUrl: this.baseUrl })
      this.$nextTick(() => {
          this.$refs.libview.treeSelect(key, e)
      })
    },
    onExpand() {
      console.log('Trigger Expand');
    },
    handheTableSearch(val,type,queryParams){
      this.repositories = []
      this.$nextTick(() => {
        this.$refs.repositoryTree.empty()
        this.$refs.repositoryTree.loadingMoreShow(true)
      })
      this.queryParams.page = 1
      if(this.isChecked){
        this.queryParams.limit = 20
      }else{
        this.queryParams.limit = 10
      }
      if(type === 'storageId'){
        const params = JSON.parse(JSON.stringify(queryParams))
        // 给当前页面搜索条赋值
        this.queryParams.layout = params.layout ? genLayoutType(params.layout) : ''
        this.queryParams.type = params.type
        const item = this.storageData.find(ele => ele.id === val)
        this.setCurrentStorage(item)
      }else{
        console.log(val,'log of val')
        // 此时的val为queryParams
        const params = JSON.parse(JSON.stringify(val))
        params.layout = val.layout ? genLayoutType(val.layout) : ''
        params.page = 1
        params.limit = 20 // 20
        // 给当前页面搜索条赋值
        this.queryParams.layout = params.layout
        this.queryParams.type = params.type
        this.getQueryStorage(params) 
      }
    },
    changeSyncEnabled(val){
      this.storageCreateData.syncEnabled = val;
    },
    message(status, type, message) {
      let statusList = [401, 403]
      if (statusList.includes(status)) {
        return
      }
      if (!message) {
        message = this.$t('Storage.OperationSuccessful')
      }
      this.$notification[type]({
        message: message,
        description: "",
      })
    },
    resetFolibRepository() {
      this.folibRepository = {
        allowsDeletion: true,
        allowsDeployment: true,
        allowsDirectoryBrowsing: true,
        allowsForceDeletion: false,
        allowsRedeployment: false,
        artifactCoordinateValidators: null,
        artifactMaxSize: 100,
        storageMaxSize: 0,
        basedir: null,
        checksumHeadersEnabled: true,
        groupRepositories: [],
        httpConnectionPool: null,
        id: "",
        layout: "",
        subLayout: "",
        policy: "release",
        proxyConfiguration: {
          host: null,
          nonProxyHosts: [],
          password: null,
          port: null,
          type: null,
          username: null
        },
        remoteRepository: {
          allowsDirectoryBrowsing: true,
          autoBlocking: true,
          autoImportRemoteSSLCertificate: false,
          checkIntervalSeconds: 60,
          checksumPolicy: 'None',
          checksumValidation: true,
          downloadRemoteIndexes: true,
          password: "",
          url: "",
          username: ""
        },
        repositoryConfiguration: null,
        secured: false,
        status: "In Service",
        storageProvider: "local",
        trashEnabled: true,
        type: "hosted",
      }
    },
    changeStorageType() {
      this.customStorage = false
      this.storagePrefix = null
    },
    changeStorageUpdateType() {
      this.customStorage = false
      this.storagePrefix = null
    },
    createHandleView() {
      checkMachineCode().then(res => {
        if (res.haveError) {
          setTimeout(() => {
            this.$notification.open({
              class: 'ant-notification-warning',
              message: this.$t('Storage.LicenseError'),
              description: this.$t('Storage.CheckLicense'),
            });
          }, 1000);
        } else {
          if (res.dalyOut) {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-warning',
                message: this.$t('Storage.LicenseExpired'),
                description: this.$t('Storage.AddWarehouseAfterRenewal'),
              });
            }, 1000);
          } else {
            this.storageMaxSize = 0
            this.showsTorageFormModal = true
            this.storagePrefix = null
            this.customStorage = false
            if (this.$refs.storageCreate) {
              this.$refs.storageCreate.resetFields()
            }
            this.getUsersList()
          }
        }
      })
    },
    updateHandleView() {
      this.getStorage(this.currentStorage.id)
      this.showStorageUpdate = true
      this.getUsersList()
    },
    proxyConfigurationHandle() {
      if (this.enableHostProxy) {
        if (!this.folibRepository.proxyConfiguration) {
          this.folibRepository.proxyConfiguration = {
            host: null,
            nonProxyHosts: [],
            password: null,
            port: null,
            type: null,
            username: null
          }
        }
      } else {
        this.folibRepository.proxyConfiguration = null
      }
    },
    copy(url) {
      var input = document.createElement("input"); // 创建input对象
      input.value = url; // 设置复制内容
      document.body.appendChild(input); // 添加临时实例
      input.select(); // 选择实例内容
      document.execCommand("Copy"); // 执行复制
      document.body.removeChild(input); // 删除临时实例
      setTimeout(() => {
        this.$notification.success({
          message: this.$t('Storage.CopySuccessful')
        })
      }, 100)
    },
    // 初始化获取集成url
  async  getBaseUrl() {
    await  getBaseUrl().then(res => {
        this.baseUrl = res
      })
    },
    filterOption(input, option) {
      return (
        option.componentOptions.children[0].text.toLowerCase().indexOf(input.toLowerCase()) >= 0
      );
    },
    storageDelHandle(e) {
      if (this.currentStorage.id != null) {
        deleteStorages(this.currentStorage, false).then(response => {
          setTimeout(() => {
            this.$notification.success({
              message: response.message,
            })
          }, 100)
          this.showStorageUpdate = false
          this.getStorages()
          this.currentStorage = this.currentDefultStorage
          this.reload()
        }).catch(err => {
          let error = err.response.data?err.response.data:this.$t('Storage.UnknownError')
          this.$notification["error"]({
            message: error,
          })
        })
      }
    },
    storageForceDelHandle() {
      if (this.currentStorage.id != null) {
        deleteStorages(this.currentStorage, true).then(response => {
          setTimeout(() => {
            this.$notification.success({
              message: response.message,
            })
          }, 100)
          this.showStorageUpdate = false;
          this.getStorages()
          this.currentStorage = this.currentDefultStorage
          this.reload()
        }).catch(err => {
          let error = err.response.data?err.response.data:this.$t('Storage.UnknownError')
          this.$notification["error"]({
            message: error,
          })
        })
      }
    },
    handleCreateSubmit(e) {
      this.$refs.storageCreate.validate(valid => {
        if (valid) {
          if (this.storageCreateData.id != null) {
            if (this.storageCreateData.storageProvider === 's3') {
              this.storageCreateData.basedir = this.storagePrefix ? '/' + this.storagePrefix + '/' + this.storageCreateData.id : '/' + this.storageCreateData.id
            } else {
              this.storageCreateData.basedir = this.storagePrefix ? '/' + this.storagePrefix + '/' + this.storageCreateData.id : null
            }
            if (this.storageMaxSize) {
              this.storageCreateData.storageMaxSize = this.storageMaxSize * 1024 * 1024 * 1024 * 1024
            }
            createStorages(this.storageCreateData).then(response => {
              setTimeout(() => {
                this.$notification.success({
                  message: response.message,
                })
              }, 100)
              this.showsTorageFormModal = false;
              this.storageCreateData = this.storageCreateDefultData
              this.storagePrefix = null
              this.customStorage = false
              this.getStorages();
            }).catch((err) => {
              let error = JSON.stringify(err.response.data)
              let msg = error.indexOf('The storage id already exists') !== -1 ? this.$t('Storage.StorageNameExists') : this.$t('Storage.FailedCreate')
              this.message(err.response.status, "error", msg)
            })
          }
        } else {
          return false
        }
      })
    },
    handleUpdateSubmit(e) {
      if (this.currentStorage.id != null) {
        if (this.storageMaxSize) {
          this.currentStorage.storageMaxSize = this.storageMaxSize * 1024 * 1024 * 1024 * 1024
        }
        this.currentStorage
        updateStorages(this.currentStorage).then(response => {
          setTimeout(() => {
            this.$notification.success({
              message: response.message,
            })
          }, 100)
          this.showStorageUpdate = false
          this.customStorage = false
          this.getStorages()
        })
      }
    },
    getUsersList() {
      queryUser({username: this.userQueryParams.matchUsername}, this.userQueryParams).then(res => {
        const resData = []
        if (res.data.rows) {
          res.data.rows.forEach(item => {
            resData.push(item.username)
          })
        }
        this.userQueryParams.total = res.data.total
        if(this.userList.length <= res.data.total){
          this.userList.push(...resData)
        }
      })
    },
    async getStorages() {
      await getStorages().then(response => {
          this.storageData = response.storages;
          this.cacheStorage()
        })
    },
    userSearchChange(matchUsername){
      this.userQueryParams.matchUsername = matchUsername
      this.userList = []
      this.userQueryParams.page = 1
      this.getUsersList()
    },
    userOnChange(value,e){
      this.userQueryParams.matchUsername = value
      let data = e && e.data && e.data.attrs && e.data.attrs.data
      this.$emit('select', {...data, value, name: value})
      if (!this.userQueryParams.matchUsername) {
        this.userSearchChange('')
      }
    },
    userPopupScroll(e){
      const {target} = e;
      const scrllHeight = target.scrollHeight - target.scrollTop
      const clientHeight = target.clientHeight
      // 下拉框不下拉的时候
      if(scrllHeight ===0 && clientHeight ===0){
        this.userQueryParams.page = 1
      } else if(scrllHeight - clientHeight <= 30){
          // 下拉到底部时
          if(this.userList.length < this.userQueryParams.total){
              // 如果滑到底部，则加载下一页
              this.userQueryParams.page++
              this.getUsersList()
          }
      }
    },
    async getStorages() {
      await getStorages().then(response => {
        this.storageData = response.storages;
        this.cacheStorage()
      })
    },
    setCurrentStorage(item) {
      if (!item.admin || item.admin === '') {
        item.admin = undefined
      }
      if (!item.users || item.users.length == 0) {
        item.users = []
      }
      this.currentStorage.id = item.id
      this.currentStorage.basedir = item.basedir
      this.currentStorage.admin = item.admin
      this.currentStorage.storageProvider = item.storageProvider
      this.currentStorage.storageMaxSize = item.storageMaxSize
      if (!this.currentStorage.storageProvider) {
        this.currentStorage.storageProvider = 'local'
      }
      this.currentStorage.users = item.users
      this.customStorage = false
      this.storagePrefix = null
      if (this.currentStorage.basedir) {
        this.storagePrefix = this.currentStorage.basedir.replace("/" + this.currentStorage.id, "").replace("/", "")
      }
      this.getStorage(this.currentStorage.id)
    },
    loadMore(total){
      if(total !== this.queryParams.total && !this.$refs.repositoryTree.loadingMore){
        this.$refs.repositoryTree.loadingMoreShow(true)
        this.queryParams.page ++
        console.log('滚动加载...')
        const params = {
          storageId: this.currentStorage.id,
          layout: this.queryParams.layout,
          type: this.queryParams.type,
          limit: this.queryParams.limit,
          page: this.queryParams.page
        }
        this.getQueryStorage(params)
      }
      if(total == this.queryParams.total){
        this.$refs.repositoryTree.loadingMoreShow(false)
      }
    },
    handlerPageNum(page, pageSize){
      this.queryParams.page = page
      this.getStorage(this.currentStorage.id)
    },
    handlerPageSize(current, size){
      this.queryParams.limit = size
      this.queryParams.page = 1
      this.getStorage(this.currentStorage.id)
    },
    getQueryStorage(queryParams,type){
      queryRepositoriesByStorage(queryParams).then(res => {
        // this.$store.commit('setSwitchDisabled',false)
        this.switchDisabled = false
        if(res.status === 200){
          this.queryParams.total = res.data.total
          this.repositories = res.data.rows || []
          if(type){
            if(this.$refs.repositoryTree){
              this.$refs.repositoryTree.setKeyValue()
            }
          }
        }
      })
    },
    getStorage(id) {
      if (id) {
        // this.$store.commit('setSwitchDisabled',true)
        this.switchDisabled = true
        getLibraryFilter(id).then(response => {
          this.currentStorage.id = response.id
          this.currentStorage.basedir = response.basedir
          this.storagePrefix = null
          if (this.currentStorage.basedir) {
            this.storagePrefix = this.currentStorage.basedir.replace("/" + this.currentStorage.id, "").replace("/", "")
          }
          this.currentStorage.storageProvider = response.storageProvider
          if (response.storageMaxSize) {
            this.storageMaxSize = (response.storageMaxSize / ( 1024 * 1024 * 1024 * 1024)).toFixed(3)
          } else {
            this.storageMaxSize = 0
          }
          this.currentStorage.syncEnabled = false
          if (response.syncEnabled) {
            this.currentStorage.syncEnabled = true
          }
          if(this.$refs.repositoryTree){
            this.$refs.repositoryTree.empty()
          }
          this.currentStorage.admin = response.admin
          this.currentStorage.users = response.users
          const params = {
            storageId: this.currentStorage.id,
            layout: this.queryParams.layout,
            type: this.queryParams.type,
            limit: this.isChecked ? 20 : this.queryParams.limit,
            page: this.queryParams.page
          }
          this.getQueryStorage(params)
          // this.repositories = response.repositories
        })
      }
    },
    cacheStorage() {
      let cache = storage.get("libView_repository");
      if (!cache || !cache.item.id) {
        if (this.storageData) {
          let item = this.storageData[0]
          if (item && item.id) {
            this.setCurrentStorage(item)
            item.storageId = item.id
            storage.set("libView_repository", { item, baseUrl: this.baseUrl })
          }
        }
      }
      if (this.currentStorage.id) {
        this.currentStorage.basedir = this.storageData.filter(f => f.id === this.currentStorage.id)[0].basedir
      }
    },
    getLayoutType(item) {
      return getLayoutType(item)
    },
    genLayoutType(layout) {
      return genLayoutType(layout)
    },
    closeUserDialog() {
      this.folibVisible = false
      this.resetFolibRepository()
      this.step = 0
    },
    repositoryList() {
      let layout = this.genLayoutType(this.layoutChecked)
      this.queryData.storageId = this.currentStorage.id
      this.queryData.layout = layout
      //getLibraryByQuery(this.queryData).then(res => {
        // const tasksObj = objectToGroupRepositories(this.folibRepository.groupRepositories, res, this.folibRepository.id)
        // this.boards[0].tasks = tasksObj.enableSelect
        // this.boards[1].tasks = tasksObj.isSelect
      //})
      let rId = this.folibRepository.id
      if (!rId && this.folibRepositoryIds) {
        rId = this.folibRepositoryIds
      }
      this.getStoragesAndRepositories(layout, this.currentStorage.id + ":" + rId)
    },
    getStoragesAndRepositories(layout, excludeRepositoryId) {
      getStoragesAndRepositories({layout: layout, excludeRepositoryId: excludeRepositoryId }).then(res => {
        let repositories = []
        let id,arr
          //  判断是否有默认的仓库
        const defaultRepositoryId = this.folibRepository.groupDefaultRepository;
        if(defaultRepositoryId){
          this.groupDefaultRepository=defaultRepositoryId;
        }
        res.forEach(item => {
          if (item.children && item.children.length > 0) {
            item.children.forEach(children => {
              id = children.key.replace(",", ":")
              arr = id.split(":")
              let isDefault = false;
              if(defaultRepositoryId&&defaultRepositoryId===id){
                isDefault = true;
              }
              repositories.push({id: id, storageId: arr[0], repositoryId: arr[1], layout: children.layout, scope: children.scope,type:children.type,isDefault:isDefault})
            })
          }
        })
        const tasksObj = objectToGroupRepositories(this.folibRepository.groupRepositories, repositories, this.folibRepository.storageId + ":" + this.folibRepository.id)
        this.boards[0].tasks = tasksObj.enableSelect
        this.boards[1].tasks = tasksObj.isSelect
      })
    },
    toggleTree(){
      this.isTrashView = !this.isTrashView
    },
    folibVisibleShow() {
      checkMachineCode().then(res => {
        if (res.haveError) {
          setTimeout(() => {
            this.$notification.open({
              class: 'ant-notification-warning',
              message: this.$t('Storage.LicenseError'),
              description:  this.$t('Storage.CheckLicense'),
            });
          }, 1000);
        } else {

          if (res.dalyOut) {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-warning',
                message: this.$t('Storage.LicenseExpired'),
                description: this.$t('Storage.AddWarehouseAfterRenewal'),
              });
            }, 1000);
          } else {


            if (this.currentStorage.id) {
              this.resetFolibRepository()
              this.folibRepositoryEditDisabled = false
              this.layoutChecked = null
              this.layoutCheckedNow = null
              this.layoutName = null
              this.step = 0
              this.enableHostProxy = false
              this.folibRepositoryIds = ""
              this.folibVisible = true
            } else {
              setTimeout(() => {
                this.$notification.open({
                  class: 'ant-notification-warning',
                  message: this.$t('Storage.OperationIncorrect'),
                  description: this.$t('Storage.selectSpace'),
                });
              }, 1000);
            }
          }

        }

      })

    },

    moveStep(distance) {
        if(this.stepsStatus === "error" && distance>0){
           return ;
        }
      this.step += distance;
      if (this.step === 2 && !this.repositoryNameCheck(this.folibRepositoryIds)) {
        this.step -= distance;
        return false
      }
      if (this.step === 2 && this.folibRepository.type === "group") {
        this.repositoryList()
        this.calcHeight()
      }

    },

    // Toggle an item from the checkbox.
    toggleCheckbox(item) {
      this.layoutName = item.name
      if(item.disabled){
        this.layoutChecked = null
        this.layoutCheckedNow = item.type
      }else{
        this.layoutChecked = item.type
        this.layoutCheckedNow = item.type
      }
      if(!this.folibRepositoryEditDisabled){
        // this.moveStep(1);
      }
      
    },

    cronShowHandle(i, index) {
      if (i.isShow) {
        i.isShow = false
      } else {
        i.isShow = true
        this.cronCanSetList.splice(index, i)
      }
      this.$forceUpdate()

    },
    delCronOneSetHandle(i) {
      delCronOne(i.isSetted.uuid).then(res => {
        setTimeout(() => {
          this.$notification.open({
            class: 'ant-notification-success',
            message: this.$t('Storage.Success'),
            description: res,
          });
        }, 100)
      })
      this.crontasksListHandle()
    },
    saveCronOneSetHandle(i) {
      if (i.fields && i.isSetted) {
        if (!i.isSetted.cronExpression) {
          this.$notification.open({
            class: 'ant-notification-warning',
            message: this.$t('Storage.OperationIncorrect'),
            description: this.$t('Storage.FillInCronExpression'),
          })
          return false
        }
        let fiedsNew = []
        i.fields.forEach(f => {
          if (f.value !== null && f.value !== undefined) {
            fiedsNew.push({ name: f.name, value: f.value })
          }

        })
        i.isSetted.fields = fiedsNew
        if (i.isSetted.uuid) {
          let uuid = i.isSetted.uuid
          delete i.isSetted.uuid
          delete i.isSetted.name
          delete i.isSetted.properties
          updateCronOne(i.isSetted, uuid).then(res => {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-success',
                message: this.$t('Storage.Success'),
                description: res,
              });
            }, 100)
          }).catch((err) => {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-warning',
                message: this.$t('Storage.Failure'),
                description: err.response.data.error,
              });
            }, 100)

          })
        } else {
          creatCronOne(i.isSetted).then(res => {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-success',
                message: this.$t('Storage.Success'),
                description: res,
              });
            }, 100)
          }).catch((err) => {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-warning',
                message: this.$t('Storage.Failure'),
                description: err.response.data.error,
              });
            }, 100)

          })
        }

      }
      this.crontasksListHandle()
    },
    repositoryNameCheck(repositoryName) {
      if (!repositoryName) {
        this.$notification.open({
          class: 'ant-notification-warning',
          message: this.$t('Storage.FillInErrors'),
          description: this.$t('Storage.EnterRepository'),
        });
        return false
      }
      if (repositoryName.length < 1 || repositoryName.length > 30) {
        this.$notification.open({
          class: 'ant-notification-warning',
          message: this.$t('Storage.FillInErrors'),
          description: this.$t('Storage.RepositoryLimit'),
        })
        return false
      }
      let reg = /^(?![_.])[a-zA-Z0-9_.\\-]+$/
      let description = this.$t('Storage.RepositoryLimit')
      if (this.layoutChecked === 'docker') {
        reg = /^(?![_.])[a-z0-9_.\\-]+$/
        description = 'docker'+this.$t('Storage.RepositoryLimit')
      }
      if (reg.test(repositoryName) === false) {
        this.$notification.open({
          class: 'ant-notification-warning',
          message: this.$t('Storage.FillInErrors'),
          description: description,
        })
        return false
      }
      return true
    },
    addOrUpdateRepositorySecond(isNotSetCron,isClose) {
      if (this.repositoryNameCheck(this.folibRepositoryIds)) {
        this.addOrUpdateRepositoryHandel(isNotSetCron,isClose)
      }
    },
    addOrUpdateRepositoryHandel(isNotSetCron,isClose) {
      this.folibRepository.id = this.folibRepositoryIds
      //构建basedir
      if (this.currentStorage.storageProvider === 's3') {
        this.folibRepository.basedir = this.currentStorage.basedir + '/' + this.folibRepository.id
        this.folibRepository.storageProvider = 's3'
      } else {
        this.folibRepository.basedir = null
        this.folibRepository.storageProvider = 'local'
      }
      //将选中的layout图标转换为接口识别的
      this.folibRepository.subLayout = this.layoutChecked
      this.folibRepository.layout = genLayoutType(this.layoutChecked)
      //将组合好的仓库转为groupRepository
      if (this.step === 2 && this.folibRepository.type === 'group' && this.boards[1].tasks.length > 0) {
        this.folibRepository.groupRepositories = groupRepositoriesBuild(this.boards[1].tasks)
        // 判断是否组合库里有默认库有则添加
        if(this.folibRepository.groupRepositories.find(item=>item===this.groupDefaultRepository)){
          this.folibRepository.groupDefaultRepository=this.groupDefaultRepository;
        }else{
          this.folibRepository.groupDefaultRepository=null;
          this.defaultRepositoryId=null;
        }
        this.folibRepository.proxyConfiguration = null
        this.folibRepository.remoteRepository = null
      }
      if (this.step === 2 && this.folibRepository.type === 'proxy') {
        this.folibRepository.groupRepositories = null
        delete this.folibRepository.remoteRepository.customConfiguration
      }
      if (this.step === 1 && this.folibRepository.type === 'hosted') {
        this.folibRepository.groupRepositories = null
        this.folibRepository.proxyConfiguration = null
        this.folibRepository.remoteRepository = null
      }


      delete this.folibRepository.customConfigurations
      delete this.folibRepository.storageId
      this.folibRepositoryData.id= this.folibRepository.id;
      this.folibRepositoryData.storageId= this.currentStorage.id;
      this.folibRepositoryData.layout = this.folibRepository.layout;

      this.folibRepository.artifactMaxSize = this.artifactMaxSize * 1024 * 1024
      this.folibRepository.storageMaxSize =  this.setRepoMaxSize(this.repositoryStorageMaxSize);
      this.folibRepository.storageThreshold = this.setRepoThreshold(this.repositoryStorageThreshold);
      addOrUpdateRepository(this.currentStorage.id, this.folibRepository.id, this.folibRepository).then(res => {
        if (!res.error) {
          setTimeout(() => {
              //this.moveStep(1);
              this.stepsStatus="process";
              this.isRepoExist=false
            this.$notification.open({
              class: 'ant-notification-success',
              message: this.folibRepositoryEditDisabled ? this.$t('Storage.WarehouseModified') : this.$t('Storage.WarehouseAdded'),
              description: res.message,
            });
          }, 1000);
        }

        this.queryParams.page = 1
        this.getStorage(this.currentStorage.id)
        if (!isNotSetCron) {
          this.step = 0
          this.folibVisible = false
          this.resetFolibRepository()
        } else if (isNotSetCron) {
          if (this.folibRepository.type === 'hosted') {
            this.moveStep(1)
          } else {
            this.moveStep(1)
          }
          this.crontasksListHandle()
        }

      }).catch((err) => {
        let error = JSON.stringify(err.response.data)
        let msg = error.indexOf('The repository id already exists') !== -1 ? this.$t('Storage.RepositoryNameExists') : this.$t('Storage.FailedCreate')
        this.message(err.response.status, "error", msg)
          this.stepsStatus="error"
      }).finally(()=> {
          if(isClose === false){
              this.folibVisible = false;
              this.stepsStatus = 'process';
              this.step=0;
          }
      })

    },
    andCronSetHandle() {
      this.step = 0
      this.folibVisible = false
      this.resetFolibRepository()
      this.cronCanSetList = []
      this.cronSettedList = []
    },

    crontasksListHandle() {
      crontasksList(this.folibRepository.layout === 'Maven 2' ? 'MAVEN' : this.folibRepository.layout.toUpperCase()).then(res => {
        this.cronCanSetList = res
        crontasksByRepository(this.currentStorage.id, this.folibRepository.id).then(res => {
          //已经被设置的定时任务列表
          this.cronSettedList = res.cronTaskConfigurations

          //当前仓库可设置的全量列表
          this.cronCanSetList.forEach(c => {
            c.isShow = false
            c.isSetted = { jobClass: c.jobClass, cronExpression: '0 0 2 * * ?', oneTimeExecution: true, immediateExecution: false }

            //循环给fields添加
            c.fields.forEach(o => {
              if (o.name === 'storageId') {
                o.value = this.currentStorage.id
              } else if (o.name === 'repositoryId') {
                o.value = this.folibRepository.id
              }
            })

            //将已经设置好的properties写入给fields，便于后续update
            this.cronSettedList.forEach(s => {
              if (c.jobClass === s.jobClass) {
                c.isSetted = s;
                for (let key in s.properties) {
                  c.fields.forEach(o => {
                    if (o.name === key) {
                      o.value = s.properties[key] === 'true' ? true : s.properties[key] === 'false' ? false : s.properties[key]
                    }
                  })
                }
              }
            })




          })

          this.$forceUpdate()

        })

      })
    },
    getRepositoryResponseEntity(repositoryId) {
      getRepositoryResponseEntity(this.currentStorage.id, repositoryId).then(res => {
        if (res.id === repositoryId) {
          delete res.unionRepositoryConfiguration
          this.folibRepository = res
          if (this.folibRepository.proxyConfiguration && this.folibRepository.proxyConfiguration.host) {
            this.enableHostProxy = true
          } else {
            this.enableHostProxy = false
          }
          this.layoutChecked = getLayoutType(res)
          this.artifactMaxSize = this.folibRepository.artifactMaxSize / (1024 * 1024)
          this.folibRepositoryIds = this.folibRepository.id
          this.folibRepositoryEditDisabled = true
          this.folibVisible = true
          this.repositoryStorageMaxSize = this.getRepoMaxSize(this.folibRepository.storageMaxSize);
          this.repositoryStorageThreshold = this.getRepoThreshold(this.folibRepository.storageThreshold);
        }
      })

    },
    delRepositoryResponseEntity() {
      this.delForm.validateFields((err, values) => {
        if (!err) {
          if (this.willDelId === values.id) {
            delRepositoryResponseEntity(this.currentStorage.id, values.id, false).then(response => {
              setTimeout(() => {
                this.$notification.open({
                  class: 'ant-notification-success',
                  message: this.$t('Storage.Success'),
                  description: values.id + this.$t('Storage.Deleted'),
                });
              }, 100)
            }).catch((err) => {
              let error = err.response.data?err.response.data:this.$t('Storage.UnknownError')
              this.$notification["error"]({
                message: error,
              })
            }).finally(() => {
              this.deleteFormVisible = false;
              this.getStorage(this.currentStorage.id)
            })
          } else {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-warning',
                message: this.$t('Storage.FillInErrors'),
                description: this.$t('Storage.ContentError'),
              });
            }, 1000);
          }

        }
      });
    },
    delRepositoryResponseEntityForce() {
      this.delForm.validateFields((err, values) => {
        if (!err) {
          if (this.willDelId === values.id) {
            delRepositoryResponseEntity(this.currentStorage.id, values.id, true).then(response => {
              setTimeout(() => {
                this.$notification.open({
                  class: 'ant-notification-success',
                  message: this.$t('Storage.Success'),
                  description: values.id + this.$t('Storage.Deleted'),
                });
              }, 100)
            }).catch((err) => {
              let error = err.response.data?err.response.data:this.$t('Storage.UnknownError')
              this.$notification["error"]({
                message: error,
              })
            }).finally(() => {
              this.deleteFormVisible = false;
              this.getStorage(this.currentStorage.id)
            })
          } else {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-warning',
                message: this.$t('Storage.FillInErrors'),
                description: this.$t('Storage.ContentError'),
              });
            }, 1000);
          }

        }
      });
    },
    handleMenuClick(e, title) {
      if (e === "edit" && title !== null) {
        this.getRepositoryResponseEntity(title)
      } else if (e === "delete" && title !== null) {
        getRepositoryResponseEntity(this.currentStorage.id, title).then(res => {
          if (res.id === title) {
            this.willDelId = title
            this.deleteBtnVisible = false
            if (res.allowsDeletion) {
              this.deleteBtnVisible = true
            }
            this.forceDeleteBtnVisible = false
            if (res.allowsForceDeletion) {
              this.forceDeleteBtnVisible = true
            }
            this.deleteFormVisible = true
            if (this.$refs.delForm) {
              this.delForm.setFieldsValue({
                id: ''
              })
            }
          }
        })
      }
    },
    goToDetial(item) {
      storage.set("libView_repository", { item, baseUrl: this.baseUrl })
      this.$router.push({
        name: 'libDetial'
      })
    },
    oneTimeExecutionChange(value, item) {
      if (value && item.immediateExecution) {
        item.immediateExecution = false
      }
      this.$forceUpdate()
    },
    immediateExecutionChange(value, item) {
      if (value && item.oneTimeExecution) {
        item.oneTimeExecution = false
      }
      this.$forceUpdate()
    },
    hasStoragePermission() {
      return isAdmin() || this.currentStorage.admin === this.$store.state.user.name
    },
    getRepositoryUrl(repository) {
      let repositoryUrl = ''
      if (this.baseUrl) {
        repositoryUrl =
          this.baseUrl +
          getLayoutRepoPrefix(repository) + repository.id
        if (repository.subLayout && repository.subLayout === 'docker') {
          repositoryUrl = repositoryUrl.replace('http://','').replace('https://','')
        }
      }
      return repositoryUrl
    },
    calcHeight() {
      this.$nextTick(() => {
        let draggables = this.$refs['draggable']
        if (draggables) {
          let height1 = draggables[0].$el.offsetHeight
          let height2 = draggables[1].$el.offsetHeight
          let height = '100vh'
          if (height1 || height2) {
            height = (height1 > height2 ? height1 : height2) + 'px'
          }
          this.draggableHeight = height
        }
      });
    },
    verifyAlive() {
      if (!this.folibRepository.layout) {
        this.folibRepository.layout = genLayoutType(this.layoutChecked)
      }
      if (!this.folibRepository.id) {
        this.folibRepository.id = this.folibRepositoryIds
      }
      aliveRepository(this.currentStorage.id, this.folibRepository.id, this.folibRepository).then(res => {
        if (res.alive) {
          this.$notification["success"]({
            message: this.$t('Storage.ConnectedFine'),
          })
        } else {
          let msg = this.$t('Storage.ConnectedFailed')
          if (res.statusCode) {
            msg = msg + "，" + this.$t('Storage.ResponseStatusCode') + res.statusCode
          }
          if (res.statusCode === 404) {
            msg = msg + this.$t('Storage.warehouseTips')
          }
          this.$notification["warning"]({
            message: msg,
          })
        }
      }).catch((err) => {
        let error = err.response.data?err.response.data:this.$t('Storage.UnknownError')
        this.$notification.error({
          message: error,
        })
      })
    },
    addGitItem(){
      if (!this.folibRepository.repositoryConfiguration) {
        // 如果 repositoryConfiguration 为 null 或未定义，则初始化它
        this.folibRepository.repositoryConfiguration = { gitVCS: [],layout : "go" };
      }
      this.folibRepository.repositoryConfiguration.gitVCS.push({url:""})
    },
    delGitItem(index){
      this.folibRepository.repositoryConfiguration.gitVCS.splice(index, 1);
    },
      handleStepChange(){
        console.log("step:",this.step)
      },
      scopeChange (e) {
          this.userReset()
          this.getUsersList2()
      },
      getUsersList2() {
          this.userList = []
          let query = {storageId: this.folibRepository.storageId, repositoryId: this.folibRepository.id, scope: this.permissionForm.scope}
          repositoryEnableUsers(query).then(res => {
              this.userList = res
          })
      },
      userReset() {
          this.permissionForm.userList = Object.assign([], this.sourceUserList)
      },
      settingDrawerClose() {
          this.$emit('settingDrawerClose')
      },
      doPermission(){
        console.log("this.folibRepository:",this.folibRepository)
        this.$refs.permission.permissionFormSubmit()
          this.moveStep(1);
      },
      doUnionRepository(){
         this.$refs.unionRepository.unionRepositoryFormSubmit();
          this.moveStep(1);
      },
      doScan(){
        this.$refs.scan.scanFormSubmit()

      },
      doDrawerStatus(isClose,status){
          this.folibVisible = isClose;
          this.stepsStatus = status;
          this.step=0;
      },
    setDefaultRepository(id){
      this.groupDefaultRepository=id;
      this.i18nBoards.forEach(board=>{
        board.tasks.forEach(item=>{
          if(item.id===id){
            item.isDefault = true;
          }else{
            item.isDefault = false;
          }
        });
      });
      this.$refs.kanbanBoard.forEach(item=>{
        item.$forceUpdate();
      });
    },

    cancelDefaultRepository(){
      this.groupDefaultRepository=null;
      this.i18nBoards.forEach(board=>{
        board.tasks.forEach(item=>{
            item.isDefault = false;
        });
      });
      this.$refs.kanbanBoard.forEach(item=>{
        item.$forceUpdate();
      });
    },
    getRepoMaxSize(maxSize){
          if(maxSize){
              return (maxSize /(1024*1024*1024)).toFixed(3)
          }
          return 0;
    },
    setRepoMaxSize(maxSize){
          if(maxSize){
              let size = (maxSize *1024*1024*1024).toFixed(0)
              return size;
          }
          return 0;
   },
   setRepoThreshold(threshold){
      if(threshold){
          return  (threshold/100).toFixed(3)
      }
       return 0;
   },
   getRepoThreshold(threshold) {
          if (threshold) {
              return (threshold * 100).toFixed(3)
          }
          return 0;
   },
    enableCustomLayoutChange(val) {
      if (this.customLayoutList && val && !this.folibRepository.customLayout) {
        // this.folibRepository.customLayout = this.customLayoutList[0].artifactPathPattern
      }
    },
    queryCustomLayoutList() {
      queryCustomLayoutList().then(res => {
        this.customLayoutList = []
        if (res) {
          this.customLayoutList = res
        }
      }).finally(() => {
      })
    },

  },
    provide() {
        return {
            doDrawerStatus: this.doDrawerStatus // 提供给子组件的方法
        };
    },
};
</script>

<style lang="scss" scoped>

.kanban-board {
  min-width: 430px;
  box-shadow: none;
  background: #e9ecef;
  margin-right: 20px;

  >.ant-card-body {
    padding-bottom: 30px;
  }
}

.drawer-folib {
  min-width: 450px;
  box-shadow: none;
  background: #e9ecef;

}

.ghost-card {
  opacity: 0.5;
  background: #F7FAFC;
  border: 1px solid #4299e1;
}

.xinjian.ant-card {
  min-height: 203px;
  border:none;
  box-shadow: 0px 1px 6px 2px rgba(214, 214, 214, 0.1);
}

#storages::v-deep {
  .ant-list {
    width: 100%;
  }

  .ant-list-item-meta-avatar {
    margin-right: 8px;
  }

  .ant-list-item-meta {
    align-items: center;
  }

  .ant-list-item-meta-title {
    margin: 0;
  }

  .ant-anchor-ink::before {
    display: none;
  }

  .ant-anchor-link {
    padding: 0;
    margin-top: 8px;

    &.slectActive{
      background-color: #bae7ff !important;
    } 

    &:hover{
      background-color: #e6f7ff;
      border-radius: 8px;
    }
  }

  .ant-anchor-link a {
    width: 100%;
    border-radius: 8px;
    color: #67748e !important;
    padding: 5px 16px;
    background-color: transparent;
    transition: background-color 0.3s ease-in;
  }

  .ant-anchor-link a:hover {
    // background-color: #e6f7ff;
    cursor: pointer;
  }

  .ant-anchor-link a svg g {
    fill: #344767;
  }

  .ant-anchor-link a svg {
    margin-right: 8px;
  }

  .card-profile-head {
    margin: 0 0 24px;
  }

  .tags-field .ant-form-item-control {
    line-height: 33px;
  }

  .form-tag.ant-tag {
    border-radius: 20px;
    padding: 4px 10px;
    font-size: 12px;
    font-weight: 500;
    margin-right: 3.75px;
    margin-bottom: 3.75px;
    background-color: #3a416f;
    border: 1px solid #3a416f;
    color: #fff;
  }

  .form-tag.ant-tag .anticon-close {
    color: #fff;
    height: 16px;
    border-left: 1px solid hsla(0, 0%, 100%, .3);
    padding-left: 5px;
    padding-top: 2px;
    opacity: .75;
  }

  .form-tag.ant-tag .anticon-close:hover {
    color: #fff;
    opacity: 1;
  }

  .tags-field .ant-input {
    margin-bottom: 5px;
    margin-top: 4px;
  }

  .tags-field .ant-select {
    .ant-select-selection__choice__remove i {
      color: #fff;
      height: 16px;
      border-left: 1px solid hsla(0, 0%, 100%, .3);
      padding-left: 5px;
      padding-top: 2px;
      opacity: .75;

      &:hover {
        color: #fff;
        opacity: 1;
      }
    }

    .ant-select-selection__rendered>ul>li:not(.ant-select-search) {
      border-radius: 20px;
      padding: 2px 27px 2px 10px;
      font-size: 12px;
      font-weight: 500;
      margin-right: 3.75px;
      margin-bottom: 3.75px;
      background-color: #3a416f;
      border: 1px solid #3a416f;
      color: #fff;
      line-height: 2;
      height: 30px;
    }

    .ant-select-selection--multiple {
      padding: 8px 10px;
    }
  }

  .tabs-sliding.ant-tabs {
      overflow: hidden;
  }
}
.info-message{
  color:#fff;
  margin-left: 15px;
  background:#1890ff ;
  border-radius: 50%;
  scale: (1.3);
}

.switch-position{
  // position: fixed;
  // top: 40px;
  // left: 340px;
}
</style>
<style lang="scss">
.left_menu.ant-card{
  box-shadow: 0px 1px 6px 2px rgba(214, 214, 214, 0.1);

  .ant-card-head-wrapper{
    min-height: 50px;

    .ant-card-head-title{
      padding: 4px 0px;
    }
  }

  .ant-card-body{
    padding: 0px;
    padding-top: 0px;
  }
}
.self_tabs.tabs-sliding{
  position: relative;
  .ant-tabs-bar{
    margin: 0;
  }
  .ant-tabs-nav-wrap{
    margin-bottom: -4px;
  }
}
.fenye{
  position: absolute;
  top: 9px;
  right: 10px;
  display: flex; 
  -items: center; 
  justify-content: flex-end;
  margin-bottom:40px;
  * {
    border: none !important;
  }
  li{
    box-shadow: 0px 1px 6px 1px rgba(194, 193, 193, 0.1);
  }
  .ant-pagination-item-active{
    font-weight: 600;
    font-size: 15px;
  }
  .ant-select-selection--single{
    height: 32px;

    *{
      line-height: 30px;
    }
  }

  .ant-select-arrow{
    top: 10px;
  }

  .ant-pagination-options-size-changer.ant-select{
    margin-right: 0px;
  }
}
.text-desc{
  padding: 10px;
  padding-left: 20px;
  padding-right: 20px;
  border-radius: 8px;
  border: 1px dashed #aaa;
  // box-shadow: rgba(9, 25, 64, 0.08) 0px 2px 16px -2px, rgba(9, 25, 64, 0.1) 0px 0px 2px 0px;
  // background: rgb(250, 251, 251);
}

.switch_mode{
  // position: absolute;
  display: flex;
  justify-content: space-between;
  align-items: center;
  width: 75px;
  // background: rgba(255,255,255,0.5);
  //   box-shadow: 0px 1px 6px 1px rgba(0,0,0,0.1);
  padding: 3px;
  border-radius: 4px;
  // left: 0px;
  // margin-right: 10px;
  
  .img-sty{
    opacity: 1 !important;
    cursor: pointer;
    // background: rgba(255,255,255,0.5);
    border-radius: 4px;
    box-shadow: 0px 1px 6px 1px rgba(25, 141, 252,0.15);
    padding: 3px 5px;
    transition: all 0.3s;
  }

  .isActive{
    background: rgba(25, 141, 252,0.2);
    box-shadow: 0px 1px 6px 1px rgba(25, 141, 252,0.2);
  }
  .disabled_sty{
    width: 80px;
    height: 40px;
    position: absolute;
    cursor: not-allowed;
    z-index: 9999;
  }
}
</style>
