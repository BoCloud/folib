<!--
	This is the Settings page, it uses the dashboard layout in:
	"./layouts/Dashboard.vue" .
 -->

<template>

  <div id="storages">
    <a-row type="flex" :gutter="[24, 24]">

      <a-col :span="24" :lg="6">
        <!-- Page Anchors -->
<!--        <a-affix :offset-top="navbarFixed ? 100 : 10">-->
          <a-card :bordered="false" class="header-solid mb-24">
            <template #title>
              <a-row type="flex" align="middle">
                <a-col :span="24" :md="12" class="col-info">
                  <h6 class="font-semibold m-0">{{ $t('Storage.StorageList') }}</h6>
                </a-col>
                <a-col :span="24" :md="12" style="display: flex; align-items: center; justify-content: flex-end">
                  <a class="text-center text-muted font-bold">
                    <h3 v-if="$store.state.user.roles.indexOf('ADMIN') > -1" class="font-semibold text-muted mb-0"
                      @click="createHandleView">+</h3>
                  </a>
                </a-col>
              </a-row>
            </template>
            <a-anchor :targetOffset="navbarFixed ? 100 : 10" :affix="false">
              <a-anchor-link v-for="(item, index) in storageData" :key="index" href="javascript:void(null)"
                :class="{ slectActive: item.id === currentStorage.id }">
                <div slot="title" class="ant-list-item-meta" @click="setCurrentStorage(item)">
                  <a-icon :type="item.storageProvider === 's3' ? 'cloud' : 'appstore'" theme="filled"
                    class="text-gray-6 text-lg" />
                  <h4 class="ant-list-item-meta-title">
                    <span class="font-regular">{{ item.id }}</span>
                  </h4>
                </div>
              </a-anchor-link>
            </a-anchor>
          </a-card>
<!--        </a-affix>-->
        <!-- / Page Anchors -->

      </a-col>
      <a-col :span="24" :lg="18">
        <!-- User Profile card -->
        <a-card :bordered="false" id="profile" class="card-profile-head" :bodyStyle="{ padding: 0, }">
          <template #title>
            <a-row type="flex" align="middle">
              <a-col :span="24" :md="12" class="col-info">
                <a-avatar :size="74" shape="square" src="images/folib/storage.svg" />
                <div class="avatar-info">
                  <h4 class="font-semibold m-0">
                    <span>{{ currentStorage.id }}</span>
                    <a-tooltip placement="topLeft">
                      <template slot="title">
                        <span>{{ $t('Storage.s3Storage') }}</span>
                      </template>
                      <a-icon style="margin-left: 15px" v-if="currentStorage.storageProvider === 's3'" type="cloud"
                        theme="filled" class="text-gray-6 text-lg" />
                    </a-tooltip>
                  </h4>
                  <p>{{ baseUrl }}api/browse/{{ currentStorage.id }} <a>
                      <a-tooltip placement="topLeft">
                        <template slot="title">
                          <span>{{ $t('Storage.CopyStorageSpacePath') }}</span>
                        </template>
                        <a-icon type="copy" @click="copy(baseUrl + 'api/browse/' + currentStorage.id)" />
                      </a-tooltip>
                    </a></p>
                </div>
              </a-col>
              <a-col :span="24" :md="12" style="display: flex; align-items: center; justify-content: flex-end">
                <a-tooltip placement="topLeft">
                  <template slot="title">
                    <span>{{ $t('Storage.ModifyStorageSpace') }}</span>
                  </template>
                  <div v-if="hasStoragePermission()" @click="updateHandleView">
                    <svg width="20px" height="20px" viewBox="0 0 40 40" version="1.1" xmlns="http://www.w3.org/2000/svg"
                      xmlns:xlink="http://www.w3.org/1999/xlink">
                      <title>settings</title>
                      <g stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
                        <g transform="translate(-2020.000000, -442.000000)" class="fill-dark" fill="#FFFFFF"
                          fill-rule="nonzero">
                          <g transform="translate(1716.000000, 291.000000)">
                            <g transform="translate(304.000000, 151.000000)">
                              <polygon class="color-background" opacity="0.596981957"
                                points="18.0883333 15.7316667 11.1783333 8.82166667 13.3333333 6.66666667 6.66666667 0 0 6.66666667 6.66666667 13.3333333 8.82166667 11.1783333 15.315 17.6716667">
                              </polygon>
                              <path class="color-background"
                                d="M31.5666667,23.2333333 C31.0516667,23.2933333 30.53,23.3333333 30,23.3333333 C29.4916667,23.3333333 28.9866667,23.3033333 28.48,23.245 L22.4116667,30.7433333 L29.9416667,38.2733333 C32.2433333,40.575 35.9733333,40.575 38.275,38.2733333 L38.275,38.2733333 C40.5766667,35.9716667 40.5766667,32.2416667 38.275,29.94 L31.5666667,23.2333333 Z"
                                opacity="0.596981957"></path>
                              <path class="color-background"
                                d="M33.785,11.285 L28.715,6.215 L34.0616667,0.868333333 C32.82,0.315 31.4483333,0 30,0 C24.4766667,0 20,4.47666667 20,10 C20,10.99 20.1483333,11.9433333 20.4166667,12.8466667 L2.435,27.3966667 C0.95,28.7083333 0.0633333333,30.595 0.00333333333,32.5733333 C-0.0583333333,34.5533333 0.71,36.4916667 2.11,37.89 C3.47,39.2516667 5.27833333,40 7.20166667,40 C9.26666667,40 11.2366667,39.1133333 12.6033333,37.565 L27.1533333,19.5833333 C28.0566667,19.8516667 29.01,20 30,20 C35.5233333,20 40,15.5233333 40,10 C40,8.55166667 39.685,7.18 39.1316667,5.93666667 L33.785,11.285 Z">
                              </path>
                            </g>
                          </g>
                        </g>
                      </g>
                    </svg>
                  </div>
                </a-tooltip>
              </a-col>
            </a-row>
          </template>
        </a-card>

        <a-tabs class="tabs-sliding" default-active-key="1">
          <a-tab-pane key="1" :tab="$t('Storage.RepositoryList')">
            <a-row type="flex" :gutter="24">
              <a-col :span="8" class="mb-24" v-for="(item, index) in repositories" :key="index">
                <!-- Project Card -->
                <CardProjectFolib :title=item.id :logo="'images/folib/' + getLayoutType(item) + '.svg'"
                  :team="['images/folib/' + item.type + '.svg']" :participants="item.type" :due="item.policy"
                  :repository="item" :storageAdmin="currentStorage.admin" @handleMenuClick="handleMenuClick" @goToDetial="goToDetial(item)">
                  <a-tooltip placement="topLeft">
                    <template slot="title">
                      {{ getRepositoryUrl(item) }}
                    </template>
                    <p>http://..../{{ item.id }} <a>
                        <a-icon type="copy" @click="copy(getRepositoryUrl(item))" />
                      </a></p>
                  </a-tooltip>
                </CardProjectFolib>
                <!-- / Project Card -->
              </a-col>

              <a-col :span="8" class="mb-24" v-if="hasStoragePermission()">
                <a-card @click="folibVisibleShow()" class="crm-bar-line header-solid h-full xinjian"
                  :bodyStyle="{ padding: 0, height: '100%', display: 'flex', alignItems: 'center', justifyContent: 'center' }">
                  <a class="text-center text-muted font-bold">
                    <h3 class="font-semibold text-muted mb-0">+</h3>
                    <h5 class="font-semibold text-muted">{{ $t('Storage.createModal') }}</h5>
                  </a>
                </a-card>

              </a-col>
            </a-row>
          </a-tab-pane>
          <a-tab-pane key="2" :tab="$t('Storage.StorageOverview')" v-if="isLogin">
            <a-row type="flex" :gutter="24">
              <a-col :span="24">
                <Overview :storageId="currentStorage.id"/>
                <StorageInfo class="mt-20" :storageId="currentStorage.id"/>
              </a-col>
            </a-row>
          </a-tab-pane>
        </a-tabs>
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
              <a-select v-model="storageCreateData.admin" style="width: 100%" model="default" show-search
                :placeholder="$t('Storage.SelectAdministrator')">
                <a-select-option v-for="(tag, index) in userList" :key="index" :value="tag.username">
                  {{ tag.username }}
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
      on-ok="showStorageUpdate = false" width="50%">
      <a-form :hideRequiredMark="true">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-item class="tags-field mb-10" :label="$t('Storage.StorageSpaceName')" :colon="false">
              <a-input disabled v-model="currentStorage.id" :placeholder="$t('Storage.StorageSpaceName')">
                <a-icon slot="prefix" type="appstore" />
              </a-input>
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
              <a-select v-model="currentStorage.admin" style="width: 100%" model="default" show-search
                :placeholder="$t('Storage.SelectAdministrator')">
                <a-select-option v-for="(tag, index) in userList" :key="index" :value="tag.username">
                  {{ tag.username }}
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
                <a-step title="权限设置" :disabled="isRepoExist" />
                <a-step title="定时策略" :disabled="isRepoExist" />
                <a-step title="联邦库"  :disabled="isRepoExist"/>
                <a-step title="扫描" :disabled="isRepoExist"/>
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

            <h5 class="font-regular text-center">{{ folibRepositoryEditDisabled ? $t('Storage.ClickNext') : $t('Storage.HowToChoose') }} </h5>
            <p class="text-center">
              {{ folibRepositoryEditDisabled ? $t('Storage.WarehouseTypeRemain') : $t('Storage.IdentifyWarehouseType') }}
            </p>

            <a-form :form="form" class="mt-30" :hideRequiredMark="true">
              <a-row type="flex" :gutter="[24]">
                <a-col :span="24" :md="10" :lg="20" class="mx-auto">
                  <a-row class="checkbox-group" type="flex" :gutter="[50]">
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'raw' ? 'active' : '']"
                        @click="toggleCheckbox('raw')">
                        <a-avatar :size="44" shape="square"
                          style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/raw.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Raw</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'maven' ? 'active' : '']"
                        @click="toggleCheckbox('maven')">
                        <a-avatar :size="44" shape="square"
                          style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/maven.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Maven</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'npm' ? 'active' : '']"
                        @click="toggleCheckbox('npm')">
                        <a-avatar :size="44" shape="square"
                          style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/npm.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Npm</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'yarn' ? 'active' : '']"
                        @click="toggleCheckbox('yarn')">
                        <a-avatar :size="44" shape="square"
                          style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/yarn.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Yarn</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'nuget' ? 'active' : '']"
                        @click="toggleCheckbox('nuget')">
                        <a-avatar :size="44" shape="square"
                          style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/nuget.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>NuGet</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'ivy' ? 'active' : '']"
                        @click="toggleCheckbox('ivy')">
                        <a-avatar :size="44" shape="square"
                          style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/ivy.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Ivy</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'sbt' ? 'active' : '']"
                        @click="toggleCheckbox('sbt')">
                        <a-avatar :size="44" shape="square"
                          style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/sbt.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>SBT</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'gradle' ? 'active' : '']"
                        @click="toggleCheckbox('gradle')">
                        <a-avatar :size="44" shape="square"
                          style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/gradle.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Gradle</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'rpm' ? 'active' : '']"
                        @click="toggleCheckbox('rpm')">
                        <a-avatar :size="44" shape="square"
                          style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/rpm.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Rpm</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'docker' ? 'active' : '']"
                        @click="toggleCheckbox('docker')">
                        <a-avatar :size="44" shape="square"
                          style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/docker.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Docker</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'php' ? 'active' : '']"
                        @click="toggleCheckbox('php')">
                        <a-avatar :size="44" shape="square"
                          style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/php.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Php</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'pypi' ? 'active' : '']"
                        @click="toggleCheckbox('pypi')">
                        <a-avatar :size="44" shape="square"
                          style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/pypi.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>PyPi</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'conan' ? 'active' : '']"
                        @click="toggleCheckbox('conan')">
                        <a-avatar :size="44" shape="square"
                          style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/conan.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Conan</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'helm' ? 'active' : '']"
                        @click="toggleCheckbox('helm')">
                        <a-avatar :size="44" shape="square"
                          style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/helm.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Helm</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'cocoapods' ? 'active' : '']"
                           @click="toggleCheckbox('cocoapods')">
                        <a-avatar :size="44" shape="square"
                                  style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/cocoapods.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>CocoaPods</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'go' ? 'active' : '']"
                           @click="toggleCheckbox('go')">
                        <a-avatar :size="44" shape="square"
                                  style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/go.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Go</h6>
                    </a-col>
                    <a-col :span="4">
                        <div class="checkbox-label" :class="[layoutChecked === 'ohpm' ? 'active' : '']"
                               @click="toggleCheckbox('ohpm')">
                            <a-avatar :size="44" shape="square"
                                        style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                                <img src="images/folib/ohpm.svg" style="width: 100%;" alt="">
                            </a-avatar>
                        </div>
                        <h6>Ohpm</h6>
                    </a-col>

                    <a-col :span="4">
                        <div class="checkbox-label" :class="[layoutChecked === 'gitlfs' ? 'active' : '']"
                               @click="toggleCheckbox('gitlfs')">
                              <a-avatar :size="44" shape="square"
                                        style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                                  <img src="images/folib/gitlfs.svg" style="width: 100%;" alt="">
                              </a-avatar>
                        </div>
                        <h6>Git LFS</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'huggingface' ? 'active' : '']"
                      @click="toggleCheckbox('huggingface')">
                        <a-avatar :size="44" shape="square"
                                  style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/huggingface.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>huggingface</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'pub' ? 'active' : '']"
                        @click="toggleCheckbox('pub')">
                        <a-avatar :size="44" shape="square"
                          style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/pub.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Pub</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'debian' ? 'active' : '']"
                      @click="toggleCheckbox('debian')">
                        <a-avatar :size="44" shape="square"
                                  style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/debian.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Debian</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'gems' ? 'active' : '']">
                        <a-tooltip>
                          <template slot="title">
                            {{ $t('Storage.NextVersion') }}🤝
                          </template>
                        <a-avatar :size="44" shape="square"
                                  style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/gems.svg" style="width: 100%;" alt="">
                        </a-avatar>
                        </a-tooltip>
                      </div>
                      <h6>Gems</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label" :class="[layoutChecked === 'rust' ? 'active' : '']">
                        <a-tooltip>
                          <template slot="title">
                            {{ $t('Storage.NextVersion') }}🤝
                          </template>
                        <a-avatar :size="44" shape="square"
                                  style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                          <img src="images/folib/rust.svg" style="width: 100%;" alt="">
                        </a-avatar>
                        </a-tooltip>
                      </div>
                      <h6>Rust</h6>
                    </a-col>
                  </a-row>
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
                      default-value="hosted" v-model="folibRepository.type">
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
                  <a-form-item class="mb-10" :label="$t('Storage.WarehousePath')" :colon="false">
                    <a-input disabled :placeholder="$t('Storage.DistributedRemain')" v-model="folibRepository.basedir" />
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
                <a-col :span="4">
                  <a-form-item class="mb-10" :label="$t('Storage.RecycleBin')" :colon="false">
                    <a-checkbox v-model="folibRepository.trashEnabled">
                      {{ folibRepository.trashEnabled ? $t('Storage.On') : $t('Storage.Off') }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" :label="$t('Storage.Delete')" :colon="false">
                    <a-checkbox v-model="folibRepository.allowsDeletion">
                      {{ folibRepository.allowsDeletion ? $t('Storage.Allowed') : $t('Storage.NotAllowed') }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" :label="$t('Storage.ForcedDeletion')" :colon="false">
                    <a-checkbox v-model="folibRepository.allowsForceDeletion">
                      {{ folibRepository.allowsForceDeletion ? $t('Storage.Allowed') : $t('Storage.NotAllowed') }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" :label="$t('Storage.UploadDeploy')" :colon="false">
                    <a-checkbox v-model="folibRepository.allowsDeployment">
                      {{ folibRepository.allowsDeployment ? $t('Storage.Allowed') : $t('Storage.NotAllowed') }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" :label="$t('Storage.UploadOverlay')" :colon="false">
                    <a-checkbox v-model="folibRepository.allowsRedeployment">
                      {{ folibRepository.allowsRedeployment ? $t('Storage.Allowed') : $t('Storage.NotAllowed') }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" :label="$t('Storage.DirectoryBrowsing')" :colon="false">
                    <a-checkbox v-model="folibRepository.allowsDirectoryBrowsing">
                      {{ folibRepository.allowsDirectoryBrowsing ? $t('Storage.Allowed') : $t('Storage.NotAllowed') }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
              </a-row>
              <a-row :gutter="[24]">
                <a-col :span="12">
                  <a-button @click="moveStep(-1)" class="px-25">{{ $t('Storage.Back') }}</a-button>
                </a-col>
                <a-col :span="12" class="text-right">
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
              {{ $t('Storage.RemoteLibraryAddress') }}</p>
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
                <a-col :span="4">
                  <a-form-item class="mb-10" :label="$t('Storage.TimedCheck')" :colon="false">
                    <a-input :placeholder="$t('Storage.DefaultTime')" v-model="folibRepository.remoteRepository.checkIntervalSeconds"
                      addon-after="s" />
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" :label="$t('Storage.InspectionMechanism')" :colon="false">
                    <a-select default-value="None" v-model="folibRepository.remoteRepository.checksumPolicy">
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
                <a-col :span="4">
                  <a-form-item class="mb-10" :label="$t('Storage.LocalAgent')" :colon="false">
                    <a-checkbox v-model="enableHostProxy" @change="proxyConfigurationHandle">
                      {{ enableHostProxy ? $t('Storage.On') : $t('Storage.Off') }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="3">
                  <a-form-item class="mb-10" :label="$t('Storage.DirectoryBrowsing')" :colon="false">
                    <a-checkbox v-model="folibRepository.remoteRepository.allowsDirectoryBrowsing">
                      {{ folibRepository.remoteRepository.allowsDirectoryBrowsing ? $t('Storage.Allowed') : $t('Storage.NotAllowed') }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="3">
                  <a-form-item class="mb-10" :label="$t('Storage.AutomaticLock')" :colon="false">
                    <a-checkbox v-model="folibRepository.remoteRepository.autoBlocking">
                      {{ folibRepository.remoteRepository.autoBlocking ? $t('Storage.On') : $t('Storage.Off') }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="3">
                  <a-form-item class="mb-10" :label="$t('Storage.ChecksumCheck')" :colon="false">
                    <a-checkbox v-model="folibRepository.remoteRepository.checksumValidation">
                      {{ folibRepository.remoteRepository.checksumValidation ? $t('Storage.On') : $t('Storage.Off') }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="3">
                  <a-form-item class="mb-10" :label="$t('Storage.RemoteIndexDownload')" :colon="false">
                    <a-checkbox v-model="folibRepository.remoteRepository.downloadRemoteIndexes">
                      {{ folibRepository.remoteRepository.downloadRemoteIndexes ? $t('Storage.Download') : $t('Storage.NoDownload') }}
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
                <a-col :span="12" class="text-right">
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
                  <FolibKanbanBoard v-for="(board) in i18nBoards" :key="board.id" :board="board">
                    <draggable :list="board.tasks" :animation="200" ghost-class="ghost-card" group="tasks" ref="draggable" :style="{height: draggableHeight, overflowY: 'auto'}">
                      <FolibKanbanTask v-for="(task) in board.tasks" :key="task.id" :task="task" :boardId="board.id">
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
              <a-col :span="12" class="text-right">
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
                        <UnionRepository ref="unionRepository" :isShow="isShow" :folibRepository="this.folibRepository" :settingVisible="settingVisible" @settingDrawerClose="settingDrawerClose"></UnionRepository>
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
                        <Scan ref="scan" :isShow="isShow" :folibRepository="this.folibRepository" :settingVisible="settingVisible" @settingDrawerClose="settingDrawerClose"></Scan>
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
    aliveRepository, repositoryEnableUsers
} from "@/api/folib"
import { getUsers } from "@/api/users"
import CardProjectFolib from "@/components/Cards/CardProjectFolib"
import { getLayoutType, genLayoutType, groupRepositoriesBuild, objectToGroupRepositories } from "@/utils/layoutUtil"
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
import CronTask from "@/views/Storage/components/Cron/index.vue";
import UnionRepository from "@/views/Storage/components/UnionRepository/index.vue";
import Scan from "@/views/Storage/components/Scan/index.vue";
import Permission from "@/views/Storage/components/Permission/index.vue";

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
      },
      currentDefultStorage: {
        id: null,
        basedir: null,
        admin: undefined,
        users: [],
        storageProvider: 'local',
        storageMaxSize: 0,
        bucket: null,
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
        users: []
      },
      storageCreateDefultData: {
        id: null,
        basedir: null,
        admin: undefined,
        storageProvider: 'local',
        storageMaxSize: 0,
        bucket: null,
        users: []
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
      folibRepository: {
        allowsDeletion: true,
        allowsDeployment: true,
        allowsDirectoryBrowsing: true,
        allowsForceDeletion: false,
        allowsRedeployment: false,
        artifactCoordinateValidators: null,
        artifactMaxSize: 100,
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
      },
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
        permissionForm: {
            allowAnonymous: true,
            scope: 1,
            userList: []
        },
        settingVisible: false,
        stepsStatus:"process",
        isRepoExist:true,
        notEditPolicy:false,
    };
  },
  watch: {
    '$i18n.locale'() {
      this.$forceUpdate();
    },
      layoutChecked(newVal, oldVal) {
          console.log('Selected value changed from', oldVal, 'to', newVal);
          // 在这里处理值改变的逻辑
          if(newVal !== "maven"){
              this.folibRepository.policy="mixed"
              this.notEditPolicy=true;
          }
      }
  },
  async created() {
    this.userInfo = store.state.user
  await  this.getStorages();
  await  this.getBaseUrl();

    const params = storage.get('libView_repository')

    if (params) {
      this.currentStorage.id = params.item.storageId
    }

    if (!this.currentStorage.id && this.storageData && this.storageData.length > 0) {
      this.currentStorage.id = this.storageData[0].id
    }
    this.getStorage(this.currentStorage.id)
  },
  computed: {
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
    }
  },
  methods: {
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
      getUsers().then(res => {
        this.userList = res.users
      })
    },
  async  getStorages() {
  await    getStorages().then(response => {
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
    getStorage(id) {
      if (id) {
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
          this.currentStorage.admin = response.admin
          this.currentStorage.users = response.users
          this.repositories = response.repositories
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
        res.forEach(item => {
          if (item.children && item.children.length > 0) {
            item.children.forEach(children => {
              id = children.key.replace(",", ":")
              arr = id.split(":")
              repositories.push({id: id, storageId: arr[0], repositoryId: arr[1], layout: children.layout, scope: children.scope})
            })
          }
        })
        const tasksObj = objectToGroupRepositories(this.folibRepository.groupRepositories, repositories, this.folibRepository.storageId + ":" + this.folibRepository.id)
        this.boards[0].tasks = tasksObj.enableSelect
        this.boards[1].tasks = tasksObj.isSelect
      })
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
      this.layoutChecked = item
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
    addOrUpdateRepositorySecond(isNotSetCron) {
      if (this.repositoryNameCheck(this.folibRepositoryIds)) {
        this.addOrUpdateRepositoryHandel(isNotSetCron)
      }
    },
    addOrUpdateRepositoryHandel(isNotSetCron) {
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


        this.getStorage(this.currentStorage.id)
        if (!isNotSetCron) {
            console.log("isNotSetCron")
          this.step = 0
          this.folibVisible = false
          this.resetFolibRepository()
        } else if (isNotSetCron) {
          if (this.folibRepository.type === 'hosted') {
            this.moveStep(1)
              console.log("step", this.step)
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
      let repositoryUrl = ""
      if (this.baseUrl) {
        repositoryUrl = this.baseUrl + 'storages/' + repository.storageId + '/' + repository.id
        let layout = repository.layout.toLowerCase()
        if (layout === 'docker') {
          let baseUrlArr = this.baseUrl.split('://')
          repositoryUrl = baseUrlArr[1] + repository.storageId + '/' + repository.id
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
      }
  },
    provide() {
        return {
            doDrawerStatus: this.doDrawerStatus // 提供给子组件的方法
        };
    },
};
</script>

<style lang="scss" scoped>
.slectActive {
  background-color: #eeeeee !important;
  border-radius: 8px;
}

.kanban-board {
  min-width: 450px;
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

.xinjian {
  min-height: 203px;
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
  }

  .ant-anchor-link a {
    width: 100%;
    border-radius: 8px;
    color: #67748e !important;
    padding: 10px 16px;
    background-color: transparent;
    transition: background-color 0.3s ease-in;
  }

  .ant-anchor-link a:hover {
    background-color: #eeeeee;
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
</style>
