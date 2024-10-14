<template>
  <div id="settings">
    <a-tabs class="tabs-sliding" default-active-key="1" @change="tabChange($event)">
      <a-tab-pane key="1" :tab="$t('Setting.GlobalConfiguration')">
        <a-row type="flex" :gutter="[24, 24]">
          <a-col :span="24" :lg="6">
            <!-- Page Anchors -->
            <a-affix :offset-top="navbarFixed ? 100 : 10">
              <a-card :bordered="false" class="header-solid mb-24">
                <a-anchor :targetOffset="navbarFixed ? 100 : 10" :affix="false" @click="handleClick">
                  <a-anchor-link href="#basic">
                    <div slot="title" class="ant-list-item-meta">
                      <a-icon type="snippets" theme="filled" class="text-gray-6 text-lg" />
                      <h4 class="ant-list-item-meta-title">
                        <span class="font-regular">{{ $t('Setting.BasicInfoConfig') }}</span>
                      </h4>
                    </div>
                  </a-anchor-link>
                  <a-anchor-link href="#smtp">
                    <div slot="title" class="ant-list-item-meta">
                      <a-icon type="unlock" theme="filled" class="text-gray-6 text-lg" />
                      <h4 class="ant-list-item-meta-title">
                        <span class="font-regular">SMTP {{ $t('Setting.configuration') }}</span>
                      </h4>
                    </div>
                  </a-anchor-link>
                  <a-anchor-link href="#proxy">
                    <div slot="title" class="ant-list-item-meta">
                      <a-icon type="safety-certificate" theme="filled" class="text-gray-6 text-lg" />
                      <h4 class="ant-list-item-meta-title">
                        <span class="font-regular">{{ $t('Setting.NetworkAgentConfig') }}</span>
                      </h4>
                    </div>
                  </a-anchor-link>
                  <a-anchor-link href="#cors">
                    <div slot="title" class="ant-list-item-meta">
                      <a-icon type="dashboard" theme="filled" class="text-gray-6 text-lg" />
                      <h4 class="ant-list-item-meta-title">
                        <span class="font-regular">CORS {{ $t('Setting.configuration') }}</span>
                      </h4>
                    </div>
                  </a-anchor-link>
                  <a-anchor-link href="#advance">
                    <div slot="title" class="ant-list-item-meta">
                      <a-icon type="setting" theme="filled" class="text-gray-6 text-lg" />
                      <h4 class="ant-list-item-meta-title">
                        <span class="font-regular">{{ $t('Setting.AdvancedConfig') }}</span>
                      </h4>
                    </div>
                  </a-anchor-link>
                </a-anchor>
              </a-card>
            </a-affix>
            <!-- / Page Anchors -->

          </a-col>
          <a-col :span="24" :lg="18">
            <!-- Basic Info card -->
            <a-card :bordered="false" id="basic" class="header-solid mb-24">
              <template #title>
                <h5 class="mb-0 font-semibold">{{ $t('Setting.BasicConfig') }}</h5>
              </template>
              <a-form :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="24" :lg="8">
                    <a-form-item class="mb-10" :label="$t('Setting.ApplicationName')" :colon="false">
                      <a-input placeholder="folib" v-model="serverSettings.instanceName" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="8">
                    <a-form-item class="mb-10" :label="$t('Setting.NodeTransmissionRateLimit')" :colon="false">
                      <a-input placeholder="KB/s" type="number" v-model="serverSettings.kbps" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="8">
                    <a-form-item class="mb-10" :label="$t('Setting.ProductTransferSliceSize')" :colon="false">
                      <a-input placeholder="MB" type="number" v-model="serverSettings.sliceMbSize" />
                    </a-form-item>
                  </a-col>
                </a-row>
                <a-row :gutter="[24]">
                  <a-col :span="24" :lg="8">
                    <a-form-item class="mb-10" label="Base Url" :colon="false">
                      <a-input placeholder="http://localhot:38080" v-model="serverSettings.baseUrl" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="8">
                    <a-form-item class="mb-10" :label="$t('Setting.PortNumber')" :colon="false">
                      <a-input placeholder="38080" v-model="serverSettings.port" />
                    </a-form-item>
                  </a-col>
                </a-row>

                <p>{{ $t('Setting.Note') }}</p>
                <ul class="pl-15 text-muted">
                  <li>{{ $t('Setting.autoChangeConfigFile') }}</li>
                  <li>{{ $t('Setting.TransferSliceSize') }}</li>
                  <li>{{ $t('Setting.NodeSpeedLimit') }}</li>
                  <li>{{ $t('Setting.useBaseUrl') }}</li>
                  <li>{{ instanceName }} {{ $t('Setting.communicationPort') }}</li>
                </ul>
              </a-form>
            </a-card>
            <!-- / Basic Info card -->

            <!-- Change Password card -->
            <a-card :bordered="false" id="smtp" class="header-solid mb-24">
              <template #title>
                <h5 class="mb-0 font-semibold">SMTP {{ $t('Setting.configuration') }}</h5>
              </template>
              <a-form :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="24" :lg="12">
                    <a-form-item class="mb-10" :label="$t('Setting.username')" :colon="false">
                      <a-input :placeholder="'SMTP' + $t('Setting.username')"
                        v-model="serverSettings.smtpConfigurationForm.username" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="12">
                    <a-form-item class="mb-10" :label="$t('Setting.Password')" :colon="false">
                      <a-input-password :placeholder="'SMTP' + $t('Setting.Password')" autocomplete="new-password"
                        v-model="serverSettings.smtpConfigurationForm.password" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="12">
                    <a-form-item class="mb-10" label="HOST" :colon="false">
                      <a-input placeholder="HOST" v-model="serverSettings.smtpConfigurationForm.host" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="6">
                    <a-form-item class="mb-10" :label="$t('Setting.Port')" :colon="false">
                      <a-input :placeholder="$t('Setting.Port')" v-model="serverSettings.smtpConfigurationForm.port" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="6">
                    <a-form-item class="mb-10" :label="$t('Setting.ProtocolType')" :colon="false">
                      <a-select v-model="serverSettings.smtpConfigurationForm.connection" show-search :allowClear="true"
                        :placeholder="$t('Setting.ProtocolType')" option-filter-prop="children"
                        :filter-option="filterOption">
                        <a-select-option value="plain">
                          Plain
                        </a-select-option>
                        <a-select-option value="ssl">
                          SSL
                        </a-select-option>
                        <a-select-option value="tls">
                          TLS
                        </a-select-option>
                      </a-select>
                    </a-form-item>
                  </a-col>
                </a-row>
                <p>{{ $t('Setting.Note') }}</p>
                <ul class="pl-15 text-muted">
                  <li>{{ $t('Setting.SetSystemMail') }}</li>
                  <li>{{ $t('Setting.EmailNotification') }}</li>
                </ul>
              </a-form>
            </a-card>

            <!-- Two-factor authentication card -->
            <a-card :bordered="false" id="proxy" class="header-solid mb-24">
              <template #title>
                <h5 class="mb-0 font-semibold">{{ $t('Setting.NetworkProxy') }}</h5>
              </template>
              <a-form :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="24" :lg="12">
                    <a-form-item class="mb-10" :label="$t('Setting.username')" :colon="false">
                      <a-input :placeholder="$t('Setting.ProxyUsername')"
                        v-model="serverSettings.proxyConfigurationForm.username" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="12">
                    <a-form-item class="mb-10" :label="$t('Setting.Password')" :colon="false">
                      <a-input-password :placeholder="$t('Setting.ProxyPassword')" autocomplete="new-password"
                        v-model="serverSettings.proxyConfigurationForm.password" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="12">
                    <a-form-item class="mb-10" :label="$t('Setting.ProxyAddress')" :colon="false">
                      <a-input :placeholder="$t('Setting.ProxyAddress')"
                        v-model="serverSettings.proxyConfigurationForm.host" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="6">
                    <a-form-item class="mb-10" :label="$t('Setting.Port')" :colon="false">
                      <a-input :placeholder="$t('Setting.Port')" v-model="serverSettings.proxyConfigurationForm.port" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="6">
                    <a-form-item class="mb-10" :label="$t('Setting.Type')" :colon="false">
                      <a-select v-model="serverSettings.proxyConfigurationForm.type" show-search
                        :placeholder="$t('Setting.Type')" :allowClear="true" option-filter-prop="children"
                        :filter-option="filterOption">
                        <a-select-option value="HTTP">
                          HTTP
                        </a-select-option>
                        <a-select-option value="HTTPS">
                          HTTPS
                        </a-select-option>
                      </a-select>
                    </a-form-item>
                  </a-col>
                </a-row>
                <p>{{ $t('Setting.Note') }}</p>
                <ul class="pl-15 text-muted">
                  <li>{{ $t('Setting.NetworkCommunicationFailure') }}</li>
                  <li>{{ $t('Setting.accessPublicNetwork') }}</li>
                  <li>{{ $t('Setting.AcquisitionDependency') }}</li>
                </ul>
              </a-form>
            </a-card>

            <a-card :bordered="false" id="cors" class="header-solid mb-24">
              <template #title>
                <h5 class="mb-0 font-semibold">CORS {{ $t('Setting.configuration') }}</h5>
              </template>
              <a-form :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="24" :lg="16">
                    <a-form-item class="tags-field mb-10" label="Origins" :colon="false">
                      <a-select mode="tags" v-model="serverSettings.corsConfigurationForm.allowedOrigins"
                        @change="allowedOriginsChange" style="width: 100%" :placeholder="$t('Setting.ForExample') + '*'">
                      </a-select>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="8">
                    <a-form-item class="mb-10" :label="$t('Setting.TurnOnAll')" :colon="false">
                      <span class="mr-15">{{ $t('Setting.TurnOn') }}</span>
                      <a-switch v-model="serverSettings.corsConfigurationForm.corsAllowAll"
                        @change="corsAllowAllChange" />
                    </a-form-item>
                  </a-col>
                </a-row>
                <p>{{ $t('Setting.Note') }}</p>
                <ul class="pl-15 text-muted">
                  <li>{{ $t('Setting.OnAllMeans') }}</li>
                </ul>
              </a-form>
            </a-card>

            <a-card :bordered="false" id="advance" class="header-solid mb-24">
              <template #title>
                <h5 class="mb-0 font-semibold">{{ $t('Setting.AdvancedConfig') }}</h5>
              </template>
              <a-form :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="24" :lg="8">
                    <a-form-item class="mb-10" :label="$t('Setting.AllowingAnonymousAccess')" :colon="false">
                      <span class="mr-15">{{ $t('Setting.TurnOn') }}</span>
                      <a-switch v-model="serverSettings.advancedConfigurationForm.allowAnonymous"
                        @change="corsAllowAllChange" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="8">
                    <a-form-item class="mb-10" :label="$t('Setting.DisplayValidationFile')" :colon="false">
                      <span class="mr-15">{{ $t('Setting.TurnOn') }}</span>
                      <a-switch v-model="serverSettings.advancedConfigurationForm.showChecksum"
                        @change="corsAllowAllChange" />
                    </a-form-item>
                  </a-col>
                </a-row>
                <a-row :gutter="[24]">
                  <a-col :span="24" :lg="10">
                    <a-form-item class="mb-10" :label="$t('Setting.GlobalS3Bucket')" :colon="false">
                      <a-input v-model="serverSettings.advancedConfigurationForm.globalS3Bucket"
                        :disabled="globalS3BucketDisabled" />
                    </a-form-item>
                  </a-col>
                </a-row>
                <p>{{ $t('Setting.Note') }}</p>
                <ul class="pl-15 text-muted">
                  <li>{{ $t('Setting.OpenAllowsAnonymousAccess') }}</li>
                  <li>{{ $t('Setting.OpenDisplayVerificationFile') }}</li>
                  <li>{{ $t('Setting.GlobalS3BucketTip') }}</li>
                </ul>
              </a-form>
            </a-card>

            <a-card :bordered="false" id="delete-account" class="header-solid mb-24">
              <a-form id="components-form-demo-normal-login" class="login-form list-settings-sessions"
                :hideRequiredMark="true">
                <a-row type="flex" align="middle">
                  <a-col style="min-width: 40px;" class="text-center">
                    <!--										<a-switch></a-switch>-->
                  </a-col>
                  <a-col class="pl-15">
                    <p class="mb-0 font-semibold">{{ $t('Setting.SaveOperation') }}</p>
                    <small class="text-dark">{{ $t('Setting.saveButton') }}</small>
                  </a-col>
                  <a-col :span="24" :md="12" class="ml-auto"
                    style="display: flex; align-items: center; justify-content: flex-end">
                    <a-button @click="getServerSettings">
                      {{ $t('Setting.Cancel1') }}
                    </a-button>
                    <a-button type="danger" class="ml-10" @click="saveServerSettings">
                      {{ $t('Setting.Save1') }}
                    </a-button>
                  </a-col>
                </a-row>
              </a-form>
            </a-card>
          </a-col>
        </a-row>
      </a-tab-pane>
      <a-tab-pane key="2" :tab="$t('Setting.SecurityPolicy')">
        <!-- <a-row type="flex" :gutter="[24]"> -->
        <!-- <a-col :span="24" :lg="24"> -->
        <a-tabs class="tabs-sliding card-container" default-active-key="1" @change="vulnerabilityTabChange($event)">
          <a-tab-pane key="1" :tab="$t('Setting.TheWhiteList')">
            <a-card class="header-solid white-card" id="white">
              <!-- <template #title>
                    <p>白名单</p>
                  </template> -->
              <div class="o-btn" @click="() => (showVulnerabilitiesModal = true, vulnerabilitiesType = 1)">
                <img src="images/folib/white.svg" />
              </div>
              <div class="white-group">
                <a-list item-layout="vertical" size="large" :data-source="vulnerabilities.whiteList"
                  :pagination="vulnerabilities.whiteList.length === 0 ? false : { pageSize: 5, total: vulnerabilities.whiteList.length, showLessItems: true }">
                  <a-list-item slot="renderItem" :key="index" slot-scope="item, index">
                    <label>{{ item }}</label>
                    <template #extra>
                      <a-popconfirm :title="$t('Setting.SureRemovedWhitelist')" :ok-text="$t('Setting.BeSure')"
                        :cancel-text="$t('Setting.Cancel')" class="d-popconfirm" @confirm="removeWhite(item)">
                        <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                          <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                            d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                            fill="#111827" />
                        </svg>
                        <span class="text-danger">DELETE</span>
                      </a-popconfirm>
                    </template>
                  </a-list-item>
                </a-list>
              </div>
            </a-card>
          </a-tab-pane>
          <a-tab-pane key="2" :tab="$t('Setting.Blacklist')">
            <a-card class="header-solid black-card" id="black">
              <!-- <template #title>
                    <p>黑名单</p>
                  </template> -->
              <div class="o-btn o-black" @click="() => (showVulnerabilitiesModal = true, vulnerabilitiesType = 2)">
                <img src="images/folib/black.svg" />
              </div>
              <div class="black-group">
                <a-list item-layout="vertical" size="large" :data-source="vulnerabilities.blackList"
                  :pagination="vulnerabilities.blackList.length === 0 ? false : { pageSize: 5, total: vulnerabilities.blackList.length, showLessItems: true }">
                  <a-list-item slot="renderItem" :key="index" slot-scope="item, index">
                    {{ item }}
                    <template #extra>
                      <a-popconfirm :title="$t('Setting.SureRemovedBlacklisted')" :ok-text="$t('Setting.BeSure')"
                        :cancel-text="$t('Setting.Cancel')" class="d-popconfirm" @confirm="removeBlack(item)">
                        <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                          <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                            d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                            fill="#111827" />
                        </svg>
                        <span class="text-danger">DELETE</span>
                      </a-popconfirm>
                    </template>
                  </a-list-item>
                </a-list>
              </div>
            </a-card>
          </a-tab-pane>
          <a-tab-pane key="3" :tab="$t('Setting.NotificationSet')">
            <a-card class="header-solid" id="notice">
              <a-form :form="ruleForm" ref="ruleForm" layout="vertical" :wrapper-col="{ span: 10 }"
                @submit.prevent="ruleFormSubmit">
                <a-row :gutter="[24]">
                  <a-col :span="24">
                    <a-form-item class="mb-10" :label="$t('Setting.NotifyVulnerabilityLevel')">
                      <a-checkbox-group v-decorator="['levels',
                        {
                          rules: [
                            { required: true, message: $t('Setting.SelectVulnerabilityLevel'), type: 'array' },
                          ],
                        },
                      ]" style="width: 100%;">
                        <a-row>
                          <a-col :span="6">
                            <a-checkbox value="CRITICAL">
                              {{ $t('Setting.Seriously') }}
                            </a-checkbox>
                          </a-col>
                          <a-col :span="6">
                            <a-checkbox value="HIGH">
                              {{ $t('Setting.HighRisk') }}
                            </a-checkbox>
                          </a-col>
                          <a-col :span="6">
                            <a-checkbox value="MEDIUM">
                              {{ $t('Setting.MediumRisk') }}
                            </a-checkbox>
                          </a-col>
                          <a-col :span="6">
                            <a-checkbox value="LOW">
                              {{ $t('Setting.LowRisk') }}
                            </a-checkbox>
                          </a-col>
                        </a-row>
                      </a-checkbox-group>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" class="text-left">
                    <a-form-item class="mb-10" :label="$t('Setting.NotificationPolicy')">
                      <a-checkbox-group v-decorator="['notifyScopes',
                      ]" style="width: 100%;">
                        <a-row>
                          <a-col :span="12">
                            <a-checkbox value="admin">
                              {{ $t('Setting.NotifyPlatformAdministrator') }}
                            </a-checkbox>
                          </a-col>
                          <a-col :span="12" class="text-right">
                            <a-checkbox value="storageAdmin">
                              {{ $t('Setting.NotifyStorageAdministrator') }}
                            </a-checkbox>
                          </a-col>
                        </a-row>
                      </a-checkbox-group>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24">
                    <a-form-item class="tags-field mb-10" :label="$t('Setting.DesignatedUser')">
                      <a-select mode="multiple" style="width: 100%" show-search :placeholder="$t('Setting.selectUser')"
                        v-decorator="['receiverUsers',
                        ]">
                        <a-select-option v-for="(user, index) in userList" :key="index" :value="user.username">
                          {{ user.username }}
                        </a-select-option>
                      </a-select>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24">
                    <a-form-item class="tags-field mb-10" :label="$t('Setting.SpecifyEmailAddress')">
                      <a-select mode="tags" style="width: 100%" notFoundContent="" :placeholder="$t('Setting.EnterEmail')"
                        v-decorator="['receiverEmails',
                        ]">
                      </a-select>
                    </a-form-item>
                  </a-col>
                  <a-col :span="12" class="text-right">
                    <a-button key="submit" class="px-30" size="small" type="primary" htmlType="submit">{{
                      $t('Setting.Save') }}</a-button>
                  </a-col>
                  <a-col :span="12" class="text-left">
                    <a-button key="back" class="px-30 ml-10" size="small" @click="ruleFormCancel()">{{
                      $t('Setting.Cancel') }}</a-button>
                  </a-col>
                </a-row>
              </a-form>
            </a-card>
          </a-tab-pane>
          <a-tab-pane key="4" :tab="$t('Setting.BlockingSet')">
            <a-card>
              <a-row :gutter="16">
                <a-col :span="18">
                  <a-card class="header-solid block block-settings">
                    <a-form class="block-form" ref="blockForm" :form="blockForm" layout="horizontal"
                      :wrapper-col="{ span: 24 }" @submit.prevent="blockFormSubmit">
                      <a-row :gutter="[24]">
                        <a-col :span="24" class="text-left">
                          <a-form-item class="mb-10" label="" :wrapper-col="{ span: 24 }">
                            <a-radio-group v-decorator="['blockType',
                              {
                                rules: [
                                  { required: true, message: $t('Setting.SelectBlockingMode') },
                                ],
                              },
                            ]" style="width: 100%;" @change="blockTypeChange($event.target.value)">
                              <a-row>
                                <a-col :span="6">
                                  <a-radio :value="1">
                                    {{ $t('Setting.FullBlock') }} <span class="tips">{{ $t('Setting.autoFilterBlacklist')
                                    }}</span>
                                  </a-radio>
                                </a-col>
                                <a-row class="block-full mt-30">
                                  <a-col :span="24">
                                    <a-form-item class="" :label="$t('Setting.VulnerabilityLevel')"
                                      :label-col="{ span: 4 }" :wrapper-col="{ span: 14 }">
                                      <a-checkbox-group v-decorator="['blockLevels',
                                        {
                                          rules: [
                                            { required: false, message: $t('Setting.SelectVulnerabilityLevel'), type: 'array' },
                                          ],
                                        },
                                      ]" style="width: 100%;">
                                        <a-row>
                                          <a-col :span="6">
                                            <a-checkbox value="CRITICAL">
                                              {{ $t('Setting.Seriously') }}
                                            </a-checkbox>
                                          </a-col>
                                          <a-col :span="6">
                                            <a-checkbox value="HIGH">
                                              {{ $t('Setting.HighRisk') }}
                                            </a-checkbox>
                                          </a-col>
                                          <a-col :span="6">
                                            <a-checkbox value="MEDIUM">
                                              {{ $t('Setting.MediumRisk') }}
                                            </a-checkbox>
                                          </a-col>
                                          <a-col :span="6">
                                            <a-checkbox value="LOW">
                                              {{ $t('Setting.LowRisk') }}
                                            </a-checkbox>
                                          </a-col>
                                        </a-row>
                                      </a-checkbox-group>
                                    </a-form-item>
                                  </a-col>
                                  <a-col :span="24">
                                    <a-form-item class="" :label="$t('Setting.FilteringWhitelist')"
                                      :label-col="{ span: 4 }" :wrapper-col="{ span: 1 }">
                                      <a-switch v-decorator="['filterWhites',
                                        {
                                          valuePropName: 'checked',
                                          rules: [
                                            { required: false },
                                          ],
                                        },
                                      ]" style="width: 100%;">

                                      </a-switch>
                                    </a-form-item>
                                  </a-col>
                                </a-row>
                                <a-col class="mt-30" :span="24">
                                  <a-radio :value="2">
                                    {{ $t('Setting.BlacklistBlocking') }} <span class="tips">{{
                                      $t('Setting.autoFilterWhitelist') }}</span>
                                  </a-radio>
                                </a-col>
                                <a-col class="mt-30" :span="24">
                                  <a-radio :value="3">
                                    {{ $t('Setting.PackageNameBlocking') }} <span class="tips">{{
                                      $t('Setting.InterceptByPacketName') }}</span>
                                  </a-radio>
                                  <a-tooltip v-if="packageNameShow" @click="packageNameModalShow">
                                    <template slot="title">{{ $t('Setting.Add') }}</template>
                                    <a-icon type="plus-circle" theme="filled" class="cursor-pointer package-name-add"
                                      :style="{ fontSize: '28px', color: '#1890FF' }" />
                                  </a-tooltip>
                                </a-col>
                              </a-row>
                            </a-radio-group>
                          </a-form-item>
                        </a-col>
                        <a-col :span="24">
                          <package-name v-if="packageNameShow" ref="packageName" />
                        </a-col>
                        <a-col :span="12" class="text-right mt-50">
                          <a-button key="submit" class="px-30" size="small" type="primary" htmlType="submit">{{
                            $t('Setting.Save') }}</a-button>
                        </a-col>
                        <a-col :span="12" class="text-left mt-50">
                          <a-button key="back" class="px-30 ml-10" size="small" @click="blockFormCancel()">{{
                            $t('Setting.Cancel') }}</a-button>
                        </a-col>
                      </a-row>
                    </a-form>
                  </a-card>
                </a-col>
              </a-row>
            </a-card>
          </a-tab-pane>
        </a-tabs>
        <!-- </a-col> -->
        <!-- </a-row> -->
      </a-tab-pane>
      <a-tab-pane key="3" :tab="'LDAP ' + $t('Setting.configuration')">
        <div class="mx-auto mt-50" style="max-width: 1000px;">
          <div class="mb-50" style="max-width: 1000px;">

            <a-steps progress-dot v-model="step">
              <a-step :title="$t('Setting.ConnectionConfig')" />
              <a-step :title="$t('Setting.UserMapping')" />
              <a-step :title="$t('Setting.RoleMapping')" />
            </a-steps>
          </div>

          <div class="mb-24">
            <!-- Step 1 -->
            <a-card v-if="step == 0" :bordered="false" class="header-solid" :bodyStyle="{ paddingTop: 0 }"
              :headStyle="{ paddingBottom: '0' }">
              <template #title>
                <h5 class="mb-0">{{ $t('Setting.ConnectionConfig') }}</h5>
                <p class="font-regular">{{ $t('Setting.LDAPConnection') }}</p>
              </template>
              <a-form @submit="handleSubmit" :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="24">
                    <a-form-item class="mb-10" label="URL" :colon="false">
                      <a-input :placeholder="$t('Setting.ForExample') + 'ldap://1.2.3.4/dc=domain,dc=com'"
                        v-model="ldap.url" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-item class="mb-10" :label="$t('Setting.BindingDN')" :colon="false">
                      <a-input :placeholder="$t('Setting.ForExample') + ':cn=manager,ou=users,dc=domain,dc=com'"
                        v-model="ldap.managerDn" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-item class="mb-10" :label="$t('Setting.BindingPasswords')" :colon="false">
                      <a-input-password placeholder="********" v-model="ldap.managerPassword" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-item class="mb-10" :label="$t('Setting.EnableLDAPServer')" :colon="false">
                      <span class="mr-15">{{ ldap.enableProvider ? $t('Setting.TurnOn') : $t('Setting.ShutDown') }}</span>
                      <a-switch default-checked v-model="ldap.enableProvider" />
                    </a-form-item>
                  </a-col>
                </a-row>
                <a-row :gutter="[24]">
                  <a-col :span="12">
                  </a-col>
                  <a-col :span="12" class="text-right">
                    <a-button type="primary" @click="moveStep(1)" class="px-25">{{ $t('Setting.NextStep') }}</a-button>
                  </a-col>
                </a-row>
              </a-form>
            </a-card>
            <!-- Step 2 -->
            <a-card v-else-if="step == 1" :bordered="false" class="header-solid" :bodyStyle="{ paddingTop: 0 }"
              :headStyle="{ paddingBottom: '0' }">
              <template #title>
                <h5 class="mb-0">{{ $t('Setting.UserMapping') }}</h5>
              </template>
              <a-form @submit="handleSubmit" :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="12">
                    <a-form-item class="mb-10" :label="$t('Setting.UsersSearchForObjects')" :colon="false">
                      <a-input :placeholder="$t('Setting.ForExample') + 'ou=Users'" v-model="ldap.userSearchBase" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-item class="mb-10" :label="$t('Setting.UserFilterCriteria')" :colon="false">
                      <a-input :placeholder="$t('Setting.ForExample') + '(uid={0})'" v-model="ldap.userSearchFilter" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="16">
                    <a-form-item class="tags-field mb-10" :label="$t('Setting.userDNList')" :colon="false">
                      <a-select mode="tags" v-model="ldap.userDnPatternList" :defaultValue="ldap.userDnPatternList"
                        style="width: 100%" :placeholder="$t('Setting.ForExample') + 'uid={0},uid={0},ou=Admins'">
                        <a-select-option v-for="(tag, index) in ldap.userDnPatternList" :key="index" :value="tag">
                          {{ tag }}
                        </a-select-option>
                      </a-select>
                    </a-form-item>
                  </a-col>
                  <a-col :span="8">
                    <a-form-item class="mb-10" :label="$t('Setting.Is64Encrypted')" :colon="false">
                      <span class="mr-15">{{ ldap.userPasswordEncoded ? $t('Setting.Yes') : $t('Setting.No') }}</span>
                      <a-switch default-checked v-model="ldap.userPasswordEncoded" />
                    </a-form-item>
                  </a-col>
                </a-row>
                <a-row :gutter="[24]">
                  <a-col :span="12">
                    <a-button @click="moveStep(-1)" class="px-25">{{ $t('Setting.Back') }}</a-button>
                  </a-col>
                  <a-col :span="12" class="text-right">
                    <a-button type="primary" @click="moveStep(1)" class="px-25">{{ $t('Setting.NextStep') }}</a-button>
                  </a-col>
                </a-row>
              </a-form>
            </a-card>
            <!-- Step 3 -->
            <a-card v-else-if="step == 2" :bordered="false" class="header-solid" :bodyStyle="{ paddingTop: 0 }"
              :headStyle="{ paddingBottom: '0' }">
              <template #title>
                <h5 class="mb-0">{{ $t('Setting.RoleMatching') }}</h5>
              </template>
              <a-form @submit="handleSubmit" :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="8">
                    <a-form-item class="mb-10" :label="'Group ' + $t('Setting.Match')" :colon="false">
                      <a-input :placeholder="$t('Setting.ForExample') + 'ou=Groups'"
                        v-model="ldap.authorities.groupSearchBase" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="8">
                    <a-form-item class="mb-10" :label="'Group ' + $t('Setting.FilteringCriteria')" :colon="false">
                      <a-input :placeholder="$t('Setting.ForExample') + '(uniqueMember={0})'"
                        v-model="ldap.authorities.groupSearchFilter" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="6">
                    <a-form-item class="mb-10" :label="$t('Setting.RoleAttributes')" :colon="false">
                      <a-input :placeholder="$t('Setting.ForExample') + 'cn,ou'"
                        v-model="ldap.authorities.groupRoleAttribute" />
                    </a-form-item>

                  </a-col>
                </a-row>
                <hr class="gradient-line">
                <a-row :gutter="[24]" v-for="(item, index) in ldap.roleMappingList" :key="index">
                  <a-col :span="12">
                    <a-form-item class="mb-10" :label="'FOLIB ' + $t('Setting.Role')" :colon="false">
                      <a-select v-model="item.folibRole">
                        <a-select-option v-for="(i, index) in assignableRoles" :key="index" :value="i.name">
                          {{ i.name }}
                        </a-select-option>
                      </a-select>
                    </a-form-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-item class="mb-10" :label="'LDAP' + $t('Role')" :colon="false">
                      <a-col :span="20">
                        <a-input :placeholder="$t('Setting.EnterLDAPRole')" v-model="item.externalRole" />
                      </a-col>
                      <a-col :span="4">
                        <a-button type="link" size="small" @click="roleMappingDelHandle(item, index)">
                          <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                            <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                              d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                              fill="#111827" />
                          </svg>
                        </a-button>
                      </a-col>
                    </a-form-item>
                  </a-col>
                </a-row>
                <a-row :gutter="[24]">
                  <a-col :span="24">
                    <a-button type="dashed" style="width: 91.66666%" @click="roleMappingAddHandle">
                      <a-icon type="plus" />
                      {{ $t('Setting.Add') }}
                    </a-button>
                  </a-col>
                </a-row>
                <hr class="gradient-line">
                <a-row :gutter="[24]">
                  <a-col :span="12">
                    <a-button @click="moveStep(-1)" class="px-25">{{ $t('Setting.Back') }}</a-button>
                  </a-col>
                  <a-col :span="12" class="text-right">
                    <a-button type="primary" class="px-25" @click="putLdap">{{ $t('Setting.Done') }}</a-button>
                  </a-col>
                </a-row>
              </a-form>
            </a-card>
          </div>
        </div>
      </a-tab-pane>
      <a-tab-pane key="4" :tab="$t('Setting.NodeLicenseConfig')">
        <a-row type="flex" :gutter="24">

          <!-- 许可信息-->
          <a-col :span="24" :md="12" class="mb-24">

            <a-card :bordered="false" class="header-solid h-full card-profile-information"
              :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }" :headStyle="{ paddingRight: 0, }">
              <template #title>
                <h6 class="font-semibold m-0">FOLIB {{ $t('Setting.LicenseInfo') }}</h6>
              </template>
              <a-button v-if="machineInfo.haveError || machineInfo.dalyOut" type="link" slot="extra"
                @click="copy(machineInfo.mac)">
                <a-icon type="copy" theme="twoTone" />
              </a-button>
              <p class="text-dark" v-if="!machineInfo.haveError && !machineInfo.dalyOut">
                {{ $t('Setting.userChoose') }}{{ instanceName }}!
                {{ $t('Setting.InWhatFollows') }}{{ instanceName }} {{ $t('Setting.provideYou') }}
                {{ $t('Setting.ITDigital') }}{{ instanceName }} {{ $t('Setting.IsWithYou') }}
              </p>
              <p class="text-dark" v-if="machineInfo.haveError && machineInfo.dalyOut">
                {{ $t('Setting.userChoose') }}{{ instanceName }} ! {{ $t('Setting.serviceHotline') }}
              </p>
              <p class="text-dark" v-if="(!machineInfo.haveError) && machineInfo.dalyOut">
                {{ $t('Setting.userChoose') }}{{ instanceName }}! {{ $t('Setting.serialNumberExpired') }}
              </p>
              <hr class="my-25">
              <a-descriptions
                :title="machineInfo.haveError ? $t('Setting.NotActivated') : (!machineInfo.haveError) && machineInfo.dalyOut ? $t('Setting.HasExpired') : $t('Setting.Activated')"
                :column="1">
                <a-descriptions-item :label="$t('Setting.MachineCode')">
                  {{ machineInfo.mac }}
                </a-descriptions-item>
                <a-descriptions-item :label="$t('Setting.VersionType')">
                  {{ machineInfo.haveError || !machineInfo.object ? $t('Setting.None') : machineInfo.object.type }}
                </a-descriptions-item>
                <a-descriptions-item :label="$t('Setting.ExpirationDate')">
                  {{ machineInfo.haveError || !machineInfo.object ? $t('Setting.None') : machineInfo.object.endDate }}
                </a-descriptions-item>
                <a-descriptions-item :label="$t('Setting.SerialNumber')">
                  {{ machineInfo.haveError || !machineInfo.object ? $t('Setting.None') : machineInfo.object.codes }}
                </a-descriptions-item>
                <a-descriptions-item :label="$t('Setting.FunctionalLevel')">
                  <a-tag> {{ machineInfo.haveError || !machineInfo.object ? $t('Setting.None') :
                    upperCase(machineInfo.object.level) }}</a-tag>
                </a-descriptions-item>
                <a-descriptions-item :label="$t('Setting.WhetherItIsActiveOrNot')">
                  <a href="http://folib.com" class="mx-5 px-5" v-if="!machineInfo.haveError">
                    <a-avatar :size="24" shape="square" src="images/folib/isactivate.svg" />
                  </a>
                  <a href="http://folib.com" class="mx-5 px-5" v-if="machineInfo.haveError">
                    <a-avatar :size="24" shape="square" src="images/folib/notactivate.svg" />
                  </a>
                </a-descriptions-item>
              </a-descriptions>
            </a-card>

          </a-col>
          <a-col :span="24" :md="12" class="mb-24">
            <a-card :bordered="false" class="header-solid h-full card-profile-information"
              :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }" :headStyle="{ paddingRight: 0, }">
              <template #title>
                <h6 class="font-semibold m-0">{{ $t('Setting.ActivateSequenceNum') }}</h6>
              </template>
              <a-upload :fileList="fileList" :multiple="false" :customRequest="uploadLicense" slot="extra">
                <a-button type="link" class="by-p-r-0">
                  <svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                    <path class="fill-muted"
                      d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                      fill="#111827" />
                    <path class="fill-muted" d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z"
                      fill="#111827" />
                  </svg>
                  {{ $t('Setting.OfflineActivated') }}
                </a-button>
              </a-upload>
              <a-button type="link" slot="extra" @click="postActivate(false)" class="by-p-r-0">
                <svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                  <path class="fill-muted"
                    d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                    fill="#111827" />
                  <path class="fill-muted" d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z"
                    fill="#111827" />
                </svg>
                {{ $t('Setting.OfficiallyActivated') }}
              </a-button>
              <a-button type="link" slot="extra" @click="postActivate(true)">
                <svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                  <path class="fill-muted"
                    d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                    fill="#111827" />
                  <path class="fill-muted" d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z"
                    fill="#111827" />
                </svg>
                {{ $t('Setting.TryItOut') }}
              </a-button>
              <p class="$color-muted">
                {{ $t('Setting.FOLIBSerialNumber') }}
              </p>
              <hr class="my-25">
              <a-form-item class="mb-10" :label="$t('Setting.SerialNumber')" :colon="false">
                <a-textarea :rows="8" :placeholder="$t('Setting.machineCode')" v-model="activateCode" />
              </a-form-item>
            </a-card>
            <!-- / Conversations Card -->

          </a-col>

        </a-row>
      </a-tab-pane>

      <!--单点登录配置-->
      <a-tab-pane key="8" :tab="'SSO ' + $t('Setting.configuration')">
        <a-card class="header-solid block">
          <a-col :span="24" :md="24" class="mb-24 text-right">

            <div class="mx-25 mb-50">
              <a-col :span="24" class="text-right">
                <a-tooltip @click="() => ssoAdd()">
                  <template slot="title">{{ $t('Setting.Add') }}</template>
                  <a-icon type="plus-circle" theme="filled" class="cursor-pointer"
                    :style="{ fontSize: '28px', color: '#1890FF' }" />
                </a-tooltip>
              </a-col>
            </div>
            <a-table :columns="i18nSsoColumns" :data-source="ssoList" :pagination="false" row-key="clientId">

              <template slot="operation" slot-scope="text, record">
                <div class="col-action">
                  <a-popconfirm :title="$t('Setting.SureDelete')" okType="danger" :ok-text="$t('Setting.BeSure')"
                    :cancel-text="$t('Setting.Cancel')" @confirm="ssoDelete(record)">
                    <a-button type="link" size="small">
                      <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                        <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                          d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                          fill="#111827" />
                      </svg>
                      <span class="text-danger">DELETE</span>
                    </a-button>
                  </a-popconfirm>
                  <a-button type="link" size="small" @click="() => ssoEdit(record)">
                    <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                      <path class="fill-muted"
                        d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                        fill="#111827" />
                      <path class="fill-muted" d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z"
                        fill="#111827" />
                    </svg>
                    <span class="text-dark">EDIT</span>
                  </a-button>
                </div>
              </template>

            </a-table>

          </a-col>

        </a-card>
      </a-tab-pane>
      <a-tab-pane key="5" :tab="$t('Setting.MetadataConfig')">
        <a-card class="header-solid block">
          <div class="mx-25 mb-50">
            <a-col :span="24" class="text-right">
              <a-tooltip @click="metadataHandler(1)">
                <template slot="title">{{ $t('Setting.Add') }}</template>
                <a-icon type="plus-circle" theme="filled" class="cursor-pointer"
                  :style="{ fontSize: '28px', color: '#1890FF' }" />
              </a-tooltip>
            </a-col>
          </div>
          <a-table :columns="i18nMetadataColumns" :scroll="{ x: true }" :data-source="metadataList"
            :row-key="(r, i) => i.toString()">
            <div slot="type" slot-scope="type">
              <span v-for="(item, index) in i18nMetadataTypes" :key="index">
                <span v-if="type === item.value">{{ item.label }}</span>
              </span>
            </div>
            <div slot="viewShow" slot-scope="viewShow">
              {{ viewShow === 1 ? $t('Setting.Display') : $t('Setting.NotToShow') }}
            </div>
            <div slot="operation" slot-scope="text, record">
              <div class="col-action">
                <a-popconfirm :title="$t('Setting.SureDelete')" okType="danger" :ok-text="$t('Setting.BeSure')"
                  :cancel-text="$t('Setting.Cancel')" @confirm="metadataHandlerDelete(record)">
                  <a-button type="link" size="small">
                    <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                      <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                        d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                        fill="#111827" />
                    </svg>
                    <span class="text-danger">DELETE</span>
                  </a-button>
                </a-popconfirm>
                <a-button type="link" size="small" @click="metadataHandler(2, record)">
                  <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                    <path class="fill-muted"
                      d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                      fill="#111827" />
                    <path class="fill-muted" d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z"
                      fill="#111827" />
                  </svg>
                  <span class="text-dark">EDIT</span>
                </a-button>
              </div>
            </div>
          </a-table>
        </a-card>
      </a-tab-pane>
      <a-tab-pane key="6" :tab="$t('Setting.NodeDistributeConfig')">
        <a-tabs class="tabs-sliding" default-active-key="1" @change="tabChange($event)">
          <a-tab-pane key="1" :tab="$t('Setting.InternalNodes')">
            <a-card class="header-solid block">
              <div class="mx-25 mb-50">
                <a-col :span="24" class="text-right">
                  <a-tooltip @click="artifactDispatchHandler(1)">
                    <template slot="title">{{ $t('Setting.Add') }}</template>
                    <a-icon type="plus-circle" theme="filled" class="cursor-pointer"
                      :style="{ fontSize: '28px', color: '#1890FF' }" />
                  </a-tooltip>
                </a-col>
              </div>
              <a-table :columns="i18nArtifactDispatchColumns" :scroll="{ x: true }" :data-source="artifactDispatchList"
                :row-key="(r, i) => i.toString()">
                <div slot="isThisCluster" slot-scope="text, record">
                  {{ record.isThisCluster === true ? $t('Setting.Yes') : $t('Setting.No') }}
                </div>
                <div slot="isSyncPrivilege" slot-scope="text, record">
                  {{ record.isSyncPrivilege === true ? $t('Setting.Yes') : $t('Setting.No') }}
                </div>
                <div slot="wsClientOnline" slot-scope="text, record">
                  <span v-if="record.wsClientOnline && record.wsClientOnline === true" class="text-success">{{
                    $t('Setting.Online') }}</span>
                  <span v-else class="text-danger">{{ $t('Setting.Offline') }}</span>
                  <!--                    {{ record.online && record.online === true ? $t('Setting.Online') : $t('Setting.Offline') }}-->
                </div>
                <div slot="autoRegister" slot-scope="text, record">
                  {{ record.autoRegister && record.autoRegister === true ? $t('Setting.Auto') : $t('Setting.Manual') }}
                </div>
                <div slot="kbps" slot-scope="text, record">
                  {{ record.kbps && record.kbps > 0 ? record.kbps + ' KB/s' : $t('Setting.NoSpeedLimit') }}
                </div>

                <div slot="operation" slot-scope="text, record">
                  <!--                    <div class="col-action" v-if="!record.autoRegister">-->
                  <div class="col-action">
                    <a-popconfirm :title="$t('Setting.SureDelete')" okType="danger" :ok-text="$t('Setting.BeSure')"
                      :cancel-text="$t('Setting.Cancel')" @confirm="artifactDispatchHandlerDelete(record)">
                      <a-button type="link" size="small">
                        <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                          <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                            d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                            fill="#111827" />
                        </svg>
                        <span class="text-danger">DELETE</span>
                      </a-button>
                    </a-popconfirm>
                    <a-button type="link" size="small" @click="artifactDispatchHandler(2, record)">
                      <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                        <path class="fill-muted"
                          d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                          fill="#111827" />
                        <path class="fill-muted"
                          d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z" fill="#111827" />
                      </svg>
                      <span class="text-dark">EDIT</span>
                    </a-button>
                  </div>
                </div>
              </a-table>
            </a-card>
          </a-tab-pane>
          <a-tab-pane key="2" :tab="$t('Setting.ExternalNodes')">
            <ExternalNode />
          </a-tab-pane>
        </a-tabs>
      </a-tab-pane>
      <a-tab-pane key="7" tab="Webhook">
        <Webhook :activeKey="activeKey"></Webhook>
      </a-tab-pane>
      <a-tab-pane key="9" :tab="$t('Setting.auditConfig')">
        <audit-config></audit-config>
      </a-tab-pane>

    </a-tabs>
    <a-modal v-model="showMetadataHandler"
      :title="handlerMetadataType === 1 ? $t('Setting.AddMetadata') : $t('Setting.ModifyingMetadata')"
      :maskClosable="false" :cancelText="$t('Setting.Cancel')" :okText="$t('Setting.BeSure')"
      @cancel="metadataHandlerCancel()" @ok="metadataHandlerConfirm()" centered>
      <a-form-model layout="horizontal" ref="metadataForm" :model="metadataForm" :rules="metadataRules"
        :hideRequiredMark="true">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Setting.MetadataKEY')" :colon="false" prop="key">
              <a-input :disabled="handlerMetadataType !== 1" :placeholder="$t('Setting.EnterMetadataKEY')"
                v-model="metadataForm.key" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Setting.MetadataTypes')" :colon="false" prop="type">
              <a-select v-model="metadataForm.type" :placeholder="$t('Setting.SelectMetadataType')" show-search
                optionFilterProp="label">
                <a-select-option v-for="(item, index) in i18nMetadataTypes" :label="item.label" :key="index"
                  :value="item.value">
                  {{ item.label }}
                </a-select-option>
              </a-select>
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Setting.WhetherToDisplay')" :colon="false" prop="viewShow">
              <a-switch v-model="metadataForm.viewShow" />
            </a-form-model-item>
          </a-col>
        </a-row>
      </a-form-model>
    </a-modal>
    <a-modal v-model="showVulnerabilitiesModal"
      :title="vulnerabilitiesType === 1 ? $t('Setting.AddWhitelist') : $t('Setting.AddBlacklist')" :maskClosable="false"
      :cancelText="$t('Setting.Cancel')" :okText="$t('Setting.BeSure')" @cancel="vulnerabilitiesModalCancel()"
      @ok="addVulnerabilities()" centered>
      <a-input v-model="uuid" :placeholder="$t('Setting.EnterVulnerabilityNum')" />
    </a-modal>

    <AddPackageName v-if="showPackageNameModal" :modelVisible="showPackageNameModal"
      @packageNameHandlerCancel="packageNameModalCancel" @packageNameRefresh="packageNameRefresh" />

    <a-modal v-model="showArtifactDispatchHandler"
      :title="handlerArtifactDispatchType === 1 ? $t('Setting.AddDistributeConfig') : $t('Setting.ModifyDistributeConfig')"
      :maskClosable="false" :cancelText="$t('Setting.Cancel')" :okText="$t('Setting.BeSure')"
      @cancel="artifactDispatchHandlerCancel()" @ok="artifactDispatchHandlerConfirm()" centered>
      <a-form-model layout="horizontal" ref="artifactDispatchForm" :model="artifactDispatchForm"
        :rules="artifactDispatchRules" :hideRequiredMark="false">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Setting.ClusterNodeEnglish')" :colon="false"
              prop="clusterEnName">
              <a-input :disabled="handlerArtifactDispatchType !== 1" :placeholder="$t('Setting.EnterClusterNodeEnglish')"
                v-model="artifactDispatchForm.clusterEnName" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Setting.ClusterChineseName')" :colon="false"
              prop="clusterCnName">
              <a-input :placeholder="$t('Setting.EnterClusterChinese')" v-model="artifactDispatchForm.clusterCnName" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Setting.ClusterNodeAddress')" :colon="false"
              prop="clusterNodeHost">
              <a-input :placeholder="$t('Setting.EnterClusterNodeAddress')"
                v-model="artifactDispatchForm.clusterNodeHost" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Setting.ClusterDescribe')" :colon="false" prop="clusterNodeDesc">
              <a-input :placeholder="$t('Setting.EnterDescribe')" v-model="artifactDispatchForm.clusterNodeDesc" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Setting.NodeTransmissionRateLimit')" :colon="false"
              prop="clusterNodeDesc">
              <a-input :placeholder="$t('Setting.EnterTransmissionLimitOfNode')" v-model="artifactDispatchForm.kbps" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Setting.distributeType')" :colon="false" prop="dispatchType">
              <a-select v-model="artifactDispatchForm.dispatchType" :placeholder="$t('Setting.SelectDistributeType')"
                show-search optionFilterProp="label">
                <a-select-option v-for="(item, index) in artifactDispatchTypes" :label="item.label" :key="index"
                  :value="item.value">
                  {{ item.label }}
                </a-select-option>
              </a-select>
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Setting.LocalCluster')" :colon="false" prop="isThisCluster">
              <a-switch v-model="artifactDispatchForm.isThisCluster" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Setting.SyncData')" :colon="false" style="position: relative"
              prop="isSyncPrivilege">
              <a-popover placement="topLeft">
                <template slot="content">
                  <p class="mb-0">{{ $t('UnionRepository.SyncData') }}</p>
                </template>
                <a style="position: absolute;top: -54px;right: -28px;" class="ml-5"><a-icon type="question-circle"
                    theme="filled" /></a>
              </a-popover>
              <a-switch v-model="artifactDispatchForm.isSyncPrivilege" />
            </a-form-model-item>
          </a-col>
        </a-row>
      </a-form-model>
    </a-modal>

    <!--新增/编辑sso 对话框-->
    <a-modal v-model="ssoDialogShow" :title="ssoActionName" :ok-text="$t('Setting.Save')"
      :cancel-text="$t('Setting.Cancel')" @ok="handleOk" @cancel="handleCancel" width="60%">
      <a-form :form="ssoform" layout="vertical">

        <a-form-item>
          <span slot="label">
            ClientId&nbsp;
            <a-tooltip :title="$t('Setting.ClientUniqueIdentifier') + $t('Setting.ClientID')" class="info-message">
              <a-icon type="question-circle-o" />
            </a-tooltip>
          </span>
          <a-input v-decorator="['clientId', { rules: [{ required: true, message: $t('Setting.EnterClientID') }] }]"
            :placeholder="$t('Setting.EnterClientID')" :disabled="ssoActionName.includes($t('Setting.Edit'))" />
        </a-form-item>
        <a-form-item>
          <span slot="label">
            ClientSecret&nbsp;
            <a-tooltip :title="$t('Setting.ClientKey')" class="info-message">
              <a-icon type="question-circle-o" />
            </a-tooltip>
          </span>
          <a-input-password
            v-decorator="['clientSecret', { rules: [{ required: false, message: $t('Setting.EnterClientKey') }] }]"
            :placeholder="$t('Setting.EnterClientKey')" />
        </a-form-item>
        <a-form-item>
          <span slot="label">
            {{ $t('Setting.ClientName') }}&nbsp;
            <a-tooltip :title="$t('Setting.CustomClientName')" class="info-message">
              <a-icon type="question-circle-o" />
            </a-tooltip>
          </span>
          <a-input v-decorator="['clientName', { rules: [{ required: true, message: $t('Setting.EnterClientName') }] }]"
            :placeholder="$t('Setting.EnterClientName')" />
        </a-form-item>


        <a-form-item>
          <span slot="label">
            {{ $t('Setting.LoginAddress') }}&nbsp;
            <a-tooltip :title="$t('Setting.ThirdPartySystemProvideLogin')" class="info-message">
              <a-icon type="question-circle-o" />
            </a-tooltip>
          </span>
          <a-input
            v-decorator="['ssoPath', { rules: [{ required: true, message: $t('Setting.EnterLoginPageAddress') }] }]"
            :placeholder="$t('Setting.EnterLoginPageAddress')" />
        </a-form-item>

        <a-form-item>
          <span slot="label">
            {{ $t('Setting.LoginRedirectAddress') }}&nbsp;
            <a-tooltip :title="$t('Setting.RedirectAddressAfterLogin')" class="info-message">
              <a-icon type="question-circle-o" />
            </a-tooltip>
          </span>
          <a-input
            v-decorator="['redirectPath', { rules: [{ required: true, message: $t('Setting.EnterRedirectAddress') }] }]"
            :placeholder="$t('Setting.EnterRedirectAddress')" />
        </a-form-item>

        <a-form-item>
          <span slot="label">
            {{ $t('Setting.LogoutAddress') }}&nbsp;
            <a-tooltip :title="$t('Setting.ClearSession')" class="info-message">
              <a-icon type="question-circle-o" />
            </a-tooltip>
          </span>
          <a-input
            v-decorator="['loginOutUrl', { rules: [{ required: true, message: $t('Setting.EnterYourLogoutAddress') }] }]"
            :placeholder="$t('Setting.EnterYourLogoutAddress')" />
        </a-form-item>

        <a-form-item>
          <span slot="label">
            {{ $t('Setting.LogOutRedirectURL') }}&nbsp;
            <a-tooltip :title="$t('Setting.RedirectURLAfterLogin')" class="info-message">
              <a-icon type="question-circle-o" />
            </a-tooltip>
          </span>
          <a-input
            v-decorator="['loginOutRedPath', { rules: [{ required: true, message: $t('Setting.EnterRedirectURL') }] }]"
            :placeholder="$t('Setting.EnterRedirectURL')" />
        </a-form-item>

        <a-form-item>
          <span slot="label">
            AccessToken {{ $t('Setting.Address') }}&nbsp;
            <a-tooltip :title="$t('Setting.GetInterfaceAddress')" class="info-message">
              <a-icon type="question-circle-o" />
            </a-tooltip>
          </span>
          <a-input
            v-decorator="['accessTokenUrl', { rules: [{ required: true, message: $t('Setting.EnterAccessTokenAddress') }] }]"
            :placeholder="$t('Setting.EnterAccessTokenAddress')" />
        </a-form-item>

        <a-form-item>
          <span slot="label">
            {{ $t('Setting.UserInfoAddress') }}&nbsp;
            <a-tooltip :title="$t('Setting.EnterUserInfoAddress')" class="info-message">
              <a-icon type="question-circle-o" />
            </a-tooltip>
          </span>
          <a-input
            v-decorator="['userInfoUrl', { rules: [{ required: false, message: $t('Setting.EnterInformationAddress') }] }]"
            :placeholder="$t('Setting.EnterInformationAddress')" />
        </a-form-item>

        <a-form-item>
          <span slot="label">
            {{ $t('Setting.UserNameField') }}&nbsp;
            <a-tooltip :title="$t('Setting.setUserNameField')" class="info-message">
              <a-icon type="question-circle-o" />
            </a-tooltip>
          </span>
          <a-input v-decorator="['username', { rules: [{ required: false, message: $t('Setting.EnterUserNameField') }] }]"
            :placeholder="$t('Setting.EnterUserNameField')" />
        </a-form-item>

        <a-form-item>
          <span slot="label">
            {{ $t('Setting.ConfigInstructions') }}&nbsp;
            <a-tooltip :title="$t('Setting.SSOConfigCustomDescribeInfo')" class="info-message">
              <a-icon type="question-circle-o" />
            </a-tooltip>
          </span>
          <a-input
            v-decorator="['desc', { rules: [{ required: false, message: $t('Setting.EnterConfigInstructions') }] }]"
            :placeholder="$t('Setting.EnterConfigInstructions')" />
        </a-form-item>

      </a-form>
    </a-modal>

  </div>
</template>

<script>
import {
  getServerSettings,
  postServerSettings,
  getLdap,
  putLdap,
  getMachineCode,
  postActivate,
  checkMachineCode,
  getMetadataConfiguration,
  globalSettingAddOrUpdateMetadata,
  globalSettingDeleteMetadata,
  getArtifactDispatchConfig,
  globalSettingArtifactDispatchConfig,
  globalSettingDelArtifactDispatchConfig,
  uploadLicenseFile
} from '@/api/settings'
import { getUsersCreateFields, getUsers } from '@/api/users'
import {
  addVulnerabilitiesWhite,
  addVulnerabilitiesBlack,
  removeVulnerabilitiesWhite,
  removeVulnerabilitiesBlack,
  saveOrUpdateVulnerabilityNotify,
  securityPolicyConfig,
  securityPolicyBlock,
  securityPolicyAddPackageName,
  securityPolicyDeletePackageName
} from '@/api/folib'

import {
  getSsoList,
  addSsoClient,
  updateSsoClient,
  deleteClient

} from '@/api/sso'
import Webhook from './components/Webhook/index.vue'
import ExternalNode from './components/ExternalNode/index.vue'
import { upperCase } from "@antv/util";
import PackageName from "./components/Package/index.vue"
import AddPackageName from "./components/Package/add.vue"
import AuditConfig from "./components/AuditConfig.vue";

export default {
  props: ['navbarFixed'],
  components: {
    Webhook,
    ExternalNode,
    PackageName,
    AddPackageName,
    AuditConfig,
  },
  data() {
    const checkClusterEnName = (rule, value, callback) => {
      if (value) {
        if (value.length < 1 || value.length > 60) {
          callback(new Error(this.$t('Setting.ClusterEnNameLengthLimit')))
        } else {
          callback()
        }
      } else if (!value) {
        callback(new Error(this.$t('Setting.EnterClusterNodeEnglish')))
      } else {
        callback()
      }
    }
    const checkKey = (rule, value, callback) => {
      if (value) {
        if (value.length < 1 || value.length > 30) {
          callback(new Error(this.$t('Setting.CheckKey')))
        } else {
          callback()
        }
      } else if (!value) {
        callback(new Error(this.$t('Setting.EnterMetadataKEY')))
      } else {
        callback()
      }
    }
    const checkClusterNodeHost = (rule, value, callback) => {
      if (!value) {
        callback(new Error(this.$t('Setting.enterNodeUrl')))
      } else {
        callback()
      }
    }
    const checkDispatchType = (rule, value, callback) => {
      if (!value) {
        callback(new Error(this.$t('Setting.CheckDispatchType')))
      } else {
        callback()
      }
    }
    const checkType = (rule, value, callback) => {
      if (!value) {
        callback(new Error(this.$t('Setting.SelectMetadataType')))
      } else {
        callback()
      }
    }
    return {
      step: 0,
      serverSettings: {
        instanceName: 'folib',
        baseUrl: 'http://localhost:38080/',
        port: 38080,
        kbps: 0,
        sliceMbSize: 0,
        advancedConfigurationForm: {
          allowAnonymous: true,
          showChecksum: true,
          globalS3Bucket: undefined,
        },
        corsConfigurationForm: { allowedOrigins: ['*'], corsAllowAll: false },
        smtpConfigurationForm: {
          host: null,
          port: null,
          username: null,
          password: null,
          connection: null
        },
        proxyConfigurationForm: {
          host: null,
          port: null,
          type: null,
          username: null,
          password: null,
          nonProxyHosts: []
        }
      },
      assignableRoles: [],
      ldap: {
        url: 'ldap://127.0.0.1:53389/dc=carlspring,dc=com',
        managerDn: 'cn=admin,dc=carlspring,dc=com',
        managerPassword: 'password',
        userPasswordEncoded: false,
        userSearchBase: 'ou=Users',
        userSearchFilter: '(uid={0})',
        roleMappingList: [{ externalRole: 'Admins', folibRole: 'ADMIN' }],
        userDnPatternList: ['uid={0},ou=Users'],
        enableProvider: false,
        authorities: {
          groupSearchBase: 'ou=Groups',
          groupSearchFilter: '(uniqueMember={0})',
          searchSubtree: true,
          groupRoleAttribute: 'cn',
          rolePrefix: '',
          convertToUpperCase: false
        }
      },
      machineInfo: { mac: null, haveError: true, dalyOut: true, object: null },
      activateCode: null,
      vulnerabilities: {
        whiteList: [],
        blackList: []
      },
      showVulnerabilitiesModal: false,
      vulnerabilitiesType: null,
      uuid: '',
      ruleForm: this.$form.createForm(this, { name: 'rule_form' }),
      blockForm: this.$form.createForm(this, { name: 'block_form' }),
      userList: [],
      artifactDispatchColumns: [
        {
          title: '集群节点英文名',
          i18nKey: 'Setting.ClusterNodeEnglish',
          dataIndex: 'clusterEnName',
          customCell: () => {
            return {
              style: {
                maxWidth: '220px',
                overflow: 'hidden',
                whiteSpace: 'nowrap',
                textOverflow: 'ellipsis',
                cursor: 'pointer'
              }
            }
          },
          customRender: (text, record) => <a-tooltip placement="topLeft" title={record.clusterEnName} >{record.clusterEnName}</a-tooltip>
        },
        {
          title: '集群中文名',
          i18nKey: 'Setting.ClusterChineseName',
          dataIndex: 'clusterCnName',
          key: 'clusterCnName',
          customCell: () => {
            return {
              style: {
                maxWidth: '220px',
                overflow: 'hidden',
                whiteSpace: 'nowrap',
                textOverflow: 'ellipsis',
                cursor: 'pointer'
              }
            }
          },
          customRender: (text, record) => <a-tooltip placement="topLeft" title={record.clusterCnName} >{record.clusterCnName}</a-tooltip>
        },
        {
          title: '节点',
          i18nKey: 'Setting.Node',
          dataIndex: 'clusterNodeHost',
          key: 'clusterNodeHost',
          customCell: () => {
            return {
              style: {
                maxWidth: '180px',
                overflow: 'hidden',
                whiteSpace: 'nowrap',
                textOverflow: 'ellipsis',
                cursor: 'pointer'
              }
            }
          },
          customRender: (text, record) => <a-tooltip placement="topLeft" title={record.clusterNodeHost} >{record.clusterNodeHost}</a-tooltip>
        },
        {
          title: '描述',
          i18nKey: 'Setting.describe',
          dataIndex: 'clusterNodeDesc',
          key: 'clusterNodeDesc',
          customCell: () => {
            return {
              style: {
                maxWidth: '220px',
                overflow: 'hidden',
                whiteSpace: 'nowrap',
                textOverflow: 'ellipsis',
                cursor: 'pointer'
              }
            }
          },
          customRender: (text, record) => <a-tooltip placement="topLeft" title={record.clusterNodeDesc} >{record.clusterNodeDesc}</a-tooltip>
        },
        {
          title: '节点传输限速',
          i18nKey: 'Setting.NodeTransmissionRateLimit',
          dataIndex: 'kbps',
          key: 'kbps',
          width: 180,
          scopedSlots: { customRender: 'kbps' }
        },
        {
          title: '分发方式',
          i18nKey: 'Setting.distributeMethod',
          dataIndex: 'dispatchType',
          key: 'dispatchType',
          width: 140
        },
        {
          title: '本集群',
          i18nKey: 'Setting.LocalCluster',
          dataIndex: 'isThisCluster',
          key: 'isThisCluster',
          width: 140,
          scopedSlots: { customRender: 'isThisCluster' }
        }, {
          title: '同步数据',
          i18nKey: 'Setting.SyncData',
          dataIndex: 'isSyncPrivilege',
          key: 'isSyncPrivilege',
          width: 140,
          scopedSlots: { customRender: 'isSyncPrivilege' }
        },
        {
          title: '在线状态',
          i18nKey: 'Setting.OnlineStatus',
          dataIndex: 'wsClientOnline',
          key: 'wsClientOnline',
          width: 140,
          scopedSlots: { customRender: 'wsClientOnline' }
        },
        {
          title: '添加方式',
          i18nKey: 'Setting.AddMethod',
          dataIndex: 'autoRegister',
          key: 'autoRegister',
          width: 160,
          scopedSlots: { customRender: 'autoRegister' }
        },
        {
          title: '操作',
          i18nKey: 'Setting.Operation',
          dataIndex: 'operation',
          width: 240,
          scopedSlots: { customRender: 'operation' }
        }
      ],
      metadataColumns: [
        {
          title: '元数据KEY',
          i18nKey: 'Setting.MetadataKEY',
          dataIndex: 'key',
          key: 'key',
          width: 200
        },
        {
          title: '元数据类型',
          i18nKey: 'Setting.MetadataTypes',
          dataIndex: 'type',
          key: 'type',
          width: 200,
          scopedSlots: { customRender: 'type' }
        },
        {
          title: '是否展示',
          i18nKey: 'Setting.WhetherToDisplay',
          dataIndex: 'viewShow',
          key: 'viewShow',
          width: 200,
          scopedSlots: { customRender: 'viewShow' }
        },
        {
          title: '操作',
          i18nKey: 'Setting.Operation',
          dataIndex: 'operation',
          width: 80,
          scopedSlots: { customRender: 'operation' }
        }
      ],
      metadataList: [],
      artifactDispatchList: [],
      showMetadataHandler: false,
      handlerMetadataType: 1,
      handlerArtifactDispatchType: 1,
      artifactDispatchForm: {
        clusterEnName: undefined,
        clusterCnName: undefined,
        clusterNodeDesc: undefined,
        clusterNodeHost: undefined,
        dispatchType: undefined,
        isThisCluster: undefined,
        kbps: 0
      },
      metadataForm: {
        key: undefined,
        type: undefined,
        viewShow: false
      },
      artifactDispatchRules: {
        clusterEnName: [
          { required: true, trigger: 'blur', validator: checkClusterEnName },
        ],
        clusterNodeHost: [
          { required: true, trigger: 'blur', validator: checkClusterNodeHost }
        ],
        dispatchType: [
          { required: true, trigger: 'blur', validator: checkDispatchType }
        ]
      },
      metadataRules: {
        key: [
          { required: true, trigger: 'blur', validator: checkKey },
        ],
        type: [
          { required: true, trigger: 'blur', validator: checkType }
        ]
      },
      artifactDispatchTypes: [
        {
          label: 'push',
          value: 'push'
        },
        {
          label: 'pull',
          value: 'pull'
        }
      ],
      metadataTypes: [
        {
          label: '数字',
          i18nKey: 'Setting.Number',
          value: 'NUMERICAL'
        },
        {
          label: '字符串',
          i18nKey: 'Setting.String',
          value: 'STRING'
        },
        {
          label: '文本',
          i18nKey: 'Setting.Text',
          value: 'TEXT'
        },
        {
          label: 'Markdown',
          value: 'MD'
        },
        {
          label: 'JSON',
          value: 'JSON'
        }
      ],
      packageNameShow: false,
      showPackageNameModal: false,
      activeKey: '1',
      showArtifactDispatchHandler: false,
      ssoform: this.$form.createForm(this, { name: 'ssoform' }),
      ssoColumns: [

        {
          title: '客户端ID',
          i18nKey: 'Setting.ClientID',
          dataIndex: 'clientId',
          key: 'clientId'
        },

        {
          title: '客户端名称',
          i18nKey: 'Setting.ClientName',
          dataIndex: 'clientName',
          key: 'clientName'
        },
        {
          title: '登录地址',
          i18nKey: 'Setting.LoginAddress',
          key: 'ssoPath',
          dataIndex: 'ssoPath'
        },
        {
          title: '重定向地址',
          i18nKey: 'Setting.RedirectAddress',
          dataIndex: 'redirectPath',
          key: 'redirectPath'
        },
        // {
        //   title: '登出地址',
        //   key: 'loginOutUrl',
        //   dataIndex: 'loginOutUrl'
        // },

        // {
        //   title: '登出重定向地址',
        //   key: 'loginOutRedPath',
        //   dataIndex: 'loginOutRedPath'
        // },
        {
          title: '描述',
          i18nKey: 'Setting.describe',
          key: 'desc',
          dataIndex: 'desc'
        },
        {
          title: '操作',
          i18nKey: 'Setting.Operation',
          dataIndex: 'operation',
          scopedSlots: { customRender: 'operation' }
        }
      ],
      instanceName: sessionStorage.getItem("instanceName") || "",
      ssoActionName: '',
      ssoDialogShow: false,
      ssoObj: {},
      ssoList: [],
      globalS3BucketDisabled: false,
      fileList: [],
    }
  },
  computed: {
    i18nArtifactDispatchColumns() {
      return this.artifactDispatchColumns.map(column => {
        if (column.i18nKey) {
          column.title = this.$t(column.i18nKey);
        }
        return column;
      })
    },
    i18nMetadataColumns() {
      return this.metadataColumns.map(column => {
        if (column.i18nKey) {
          column.title = this.$t(column.i18nKey);
        }
        return column;
      })
    },
    i18nMetadataTypes() {
      return this.metadataTypes.map(column => {
        if (column.i18nKey) {
          column.label = this.$t(column.i18nKey);
        }
        return column;
      })
    },
    i18nSsoColumns() {
      return this.ssoColumns.map(column => {
        if (column.i18nKey) {
          column.title = this.$t(column.i18nKey);
        }
        return column;
      })
    },
  },
  created() {
    this.getServerSettings()
    this.getLdap()
    this.getUsersCreateFields()
    this.getMachineCode()
    this.getVulnerabilities()
    this.getArtifactDispatchConfig()
    this.getSsoList()
  },
  methods: {
    upperCase,
    getSsoList() {
      getSsoList().then(res => {
        this.ssoList = res
      })

    },
    handleSubmit(e) {
      e.preventDefault()
      this.form.validateFields((err, values) => {
        if (!err) {
        }
      })
    },
    tabChange(key) {
      this.activeKey = key
      if (key === '2') {
        this.getVulnerabilities()
      } else if (key === '5') {
        this.getMetadataConfiguration()
      }
    },
    moveStep(distance) {
      this.step += distance
    },
    getServerSettings() {
      getServerSettings().then(res => {
        this.serverSettings = res
        let allowedOrigins = this.serverSettings.corsConfigurationForm
          .allowedOrigins
        if (
          allowedOrigins &&
          allowedOrigins.length === 1 &&
          allowedOrigins[0] === '*'
        ) {
          this.serverSettings.corsConfigurationForm.corsAllowAll = true
        } else {
          this.serverSettings.corsConfigurationForm.corsAllowAll = false
        }
        if (this.serverSettings.advancedConfigurationForm.globalS3Bucket) {
          this.globalS3BucketDisabled = true
        }
      })
    },
    saveServerSettings() {
      postServerSettings(this.serverSettings).then(res => {
        if (this.serverSettings.advancedConfigurationForm.globalS3Bucket) {
          this.globalS3BucketDisabled = true
        }
        setTimeout(() => {
          this.$notification.success({
            message: this.$t('Setting.SavedSuccess')
          })
        }, 100)
      })
    },
    handleClick(e, link) {
      e.preventDefault()
    },
    // Languages select field search method.
    filterOption(input, option) {
      return (
        option.componentOptions.children[0].text
          .toLowerCase()
          .indexOf(input.toLowerCase()) >= 0
      )
    },
    getLdap() {
      getLdap().then(res => {
        this.ldap = res
      })
    },
    putLdap() {
      putLdap(this.ldap).then(res => {
        setTimeout(() => {
          this.$notification.success({
            message: this.$t('Setting.SavedSuccess')
          })
        }, 100)
      })
    },
    getUsersCreateFields() {
      getUsersCreateFields().then(res => {
        let roles = res.formDataValues[0].values
        let roleNameList = [
          'ADMIN',
          'GENERAL',
          'ARTIFACTS_MANAGER',
          'OPEN_SOURCE_MANAGE'
        ]
        roles = roles.filter(item => roleNameList.includes(item.name))
        this.assignableRoles = roles
      })
    },
    roleMappingDelHandle(item, index) {
      this.ldap.roleMappingList.splice(index, 1)
    },
    roleMappingAddHandle() {
      this.ldap.roleMappingList.splice(this.ldap.roleMappingList.length, 0, {
        externalRole: null,
        folibRole: null
      })
    },
    getMachineCode() {
      checkMachineCode().then(res => {
        this.machineInfo = res
      })
    },
    copy(code) {
      var input = document.createElement('input') // 创建input对象
      input.value = code // 设置复制内容
      document.body.appendChild(input) // 添加临时实例
      input.select() // 选择实例内容
      document.execCommand('Copy') // 执行复制
      document.body.removeChild(input) // 删除临时实例
      // console.log(url)
      setTimeout(() => {
        this.$notification.success({
          message: this.$t('Setting.CopySuccess')
        })
      }, 100)
    },
    postActivate(isPoc) {
      if (this.activateCode) {
        postActivate(this.activateCode, isPoc).then(res => {
          if (res.rel) {
            setTimeout(() => {
              this.$notification.success({
                message: this.$t('Setting.SuccessfulActivation')
              })
              this.getMachineCode()
            }, 100)
          } else {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-warning',
                message: this.$t('Setting.ActivationFailed'),
                description: res.message
              })
            }, 100)
          }
        })
      } else {
        setTimeout(() => {
          this.$notification.open({
            class: 'ant-notification-warning',
            message: this.$t('Setting.UnableToActivate'),
            description: this.$t('Setting.NoSequenceNumberWasEntered'),
          })
        }, 100)
      }
    },
    getVulnerabilities(type) {
      securityPolicyConfig()
        .then(res => {
          if (type !== 2) {
            this.vulnerabilities.whiteList = res.whites
          }
          if (type !== 1) {
            this.vulnerabilities.blackList = res.blacks
          }
        })
        .finally(() => { })
    },
    removeWhite(uuid) {
      removeVulnerabilitiesWhite({ white: uuid })
        .then(res => {
          this.successMsg(uuid + this.$t('Setting.RemovedWhitelistSuccess'))
        })
        .finally(() => {
          this.getVulnerabilities(1)
        })
    },
    removeBlack(uuid) {
      removeVulnerabilitiesBlack({ black: uuid })
        .then(res => {
          this.successMsg(uuid + this.$t('Setting.RemovedBlacklistSuccess'))
        })
        .finally(() => {
          this.getVulnerabilities(2)
        })
    },
    addWhite(uuid) {
      addVulnerabilitiesWhite({ white: uuid })
        .then(res => {
          this.successMsg(uuid + this.$t('Setting.AddWhitelistSuccess'))
        })
        .catch(err => {
          this.$notification['error']({
            message: err.response.data.error,
            description: ''
          })
        })
        .finally(() => {
          this.uuid = ''
          this.showVulnerabilitiesModal = false
          this.getVulnerabilities(1)
        })
    },
    addBlack(uuid) {
      addVulnerabilitiesBlack({ black: uuid })
        .then(res => {
          this.successMsg(uuid + this.$t('Setting.AddBlacklistSuccess'))
        })
        .catch(err => {
          this.$notification['error']({
            message: err.response.data.error,
            description: ''
          })
        })
        .finally(() => {
          this.uuid = ''
          this.showVulnerabilitiesModal = false
          this.getVulnerabilities(2)
        })
    },
    vulnerabilitiesModalCancel() {
      this.showVulnerabilitiesModal = false
    },
    addVulnerabilities() {
      if (!this.uuid) {
        this.$notification['warning']({
          message: this.$t('Setting.EnterVulnerabilityNum'),
          description: ''
        })
        return
      }
      if (this.vulnerabilitiesType === 1) {
        this.addWhite(this.uuid)
      } else if (this.vulnerabilitiesType === 2) {
        this.addBlack(this.uuid)
      }
    },
    successMsg(message) {
      if (!message) {
        message = this.$t('Setting.OperationSuccessful')
      }
      this.$notification['success']({
        message: message,
        description: ''
      })
    },
    getSecurityPolicy() {
      securityPolicyConfig()
        .then(res => {
          this.$nextTick(() => {
            if (this.$refs.ruleForm) {
              this.ruleForm.setFieldsValue({
                levels: res.levels,
                notifyScopes: res.notifyScopes,
                receiverUsers: res.receiverUsers,
                receiverEmails: res.receiverEmails
              })
            }
            if (this.$refs.blockForm) {
              this.blockForm.setFieldsValue({
                blockType: res.blockType,
                blockLevels: res.blockLevels,
                filterWhites: res.filterWhites
              })
              if (res.blockType === 3) {
                this.packageNameShow = true
              } else {
                this.packageNameShow = false
              }
            }
          })
        })
        .finally(() => { })
    },
    vulnerabilityTabChange(key) {
      if (key === '3') {
        this.getSecurityPolicy()
        this.getUsersList()
      } else if (key === '4') {
        this.getSecurityPolicy()
      } else {
        this.getVulnerabilities()
      }
    },
    ruleFormSubmit(e) {
      e.preventDefault()
      this.ruleForm.validateFieldsAndScroll((err, values) => {
        if (!err) {
          saveOrUpdateVulnerabilityNotify(values)
            .then(res => {
              this.successMsg(this.$t('Setting.NotificationSetSavedSuccess'))
              this.getSecurityPolicy()
            })
            .finally(() => { })
        }
      })
    },
    ruleFormCancel() {
      this.ruleForm.resetFields()
      this.getSecurityPolicy()
    },
    getUsersList() {
      getUsers().then(res => {
        this.userList = res.users
      })
    },
    filterOption(input, option) {
      return (
        option.componentOptions.children[0].text
          .toLowerCase()
          .indexOf(input.toLowerCase()) >= 0
      )
    },
    blockFormSubmit(e) {
      e.preventDefault()
      this.blockForm.validateFieldsAndScroll((err, values) => {
        if (!err) {
          securityPolicyBlock(values)
            .then(res => {
              this.successMsg(this.$t('Setting.BlockingSetSavedSuccess'))
              this.getSecurityPolicy()
            })
            .finally(() => { })
        }
      })
    },
    blockFormCancel() {
      this.blockForm.resetFields()
      this.getSecurityPolicy()
    },
    blockTypeChange(blockType) {
      if (blockType !== 1) {
        this.blockForm.setFieldsValue({
          blockLevels: [],
          filterWhites: false
        })
      }
      this.packageNameShow = false
      if (blockType === 3) {
        this.packageNameShow = true
      }
    },
    getMetadataConfiguration() {
      getMetadataConfiguration()
        .then(res => {
          this.metadataList = res
        })
        .finally(() => { })
    },
    artifactDispatchFormRest() {
      if (this.$refs.artifactDispatchForm) {
        this.$refs.artifactDispatchForm.resetFields()
      }
      this.artifactDispatchForm = {
        clusterEnName: undefined,
        clusterCnName: undefined,
        clusterNodeDesc: undefined,
        clusterNodeHost: undefined,
        dispatchType: undefined,
        isThisCluster: undefined
      }
    },
    metadataFormReset() {
      if (this.$refs.metadataForm) {
        this.$refs.metadataForm.resetFields()
      }
      this.metadataForm = {
        key: undefined,
        type: undefined,
        viewShow: false
      }
    },
    metadataHandler(type, item) {
      this.metadataFormReset()
      if (item) {
        let data = Object.assign({}, item)
        if (data.viewShow === 1) {
          data.viewShow = true
        } else {
          data.viewShow = false
        }
        this.metadataForm = data
      }
      this.handlerMetadataType = type
      this.showMetadataHandler = true
    },
    artifactDispatchHandlerConfirm() {
      this.$refs.artifactDispatchForm.validate(valid => {
        if (valid) {
          let data = Object.assign({}, this.artifactDispatchForm)
          if (this.handlerArtifactDispatchType === 1) {
            let flag = this.artifactDispatchList.some(
              x => x.clusterEnName === data.clusterEnName
            )
            if (flag) {
              this.$notification['warning']({
                message: this.$t('Setting.ClusterDistributeConfigExists'),
                description: ''
              })
              return false
            }
          }
          globalSettingArtifactDispatchConfig(data)
            .then(res => {
              let prefix = this.$t('Setting.Add')
              if (this.handlerArtifactDispatchType === 2) {
                prefix = this.$t('Setting.Edit')
              }
              this.successMsg(prefix + this.$t('Setting.distributeConfigSuccessful'))
              this.artifactDispatchFormRest()
              this.showArtifactDispatchHandler = false
              this.getArtifactDispatchConfig()
            })
            .catch(err => {
              this.$notification['error']({
                message: err.response.data.error,
                description: ''
              })
            })
            .finally(() => { })
        } else {
          return false
        }
      })
    },
    metadataHandlerConfirm() {
      this.$refs.metadataForm.validate(valid => {
        if (valid) {
          let data = Object.assign({}, this.metadataForm)
          if (this.handlerMetadataType === 1) {
            let flag = this.metadataList.some(
              metadata => metadata.key === data.key
            )
            if (flag) {
              this.$notification['warning']({
                message: this.$t('Setting.MetadataKEYyExists'),
                description: ''
              })
              return false
            }
          }
          if (data.viewShow) {
            data.viewShow = 1
          } else {
            data.viewShow = 0
          }
          globalSettingAddOrUpdateMetadata(data)
            .then(res => {
              let prefix = this.$t('Setting.Add')
              if (this.handlerMetadataType === 2) {
                prefix = this.$t('Setting.Edit')
              }
              this.successMsg(prefix + this.$t('Setting.metadataConfigSuccess'))
              this.metadataFormReset()
              this.showMetadataHandler = false
              this.getMetadataConfiguration()
            })
            .finally(() => { })
        } else {
          return false
        }
      })
    },
    artifactDispatchHandlerDelete(data) {
      globalSettingDelArtifactDispatchConfig(data.clusterEnName)
        .then(res => {
          this.successMsg(this.$t('Setting.DeleteDistributeConfigSuccess'))
        })
        .catch(err => {
          this.$notification['error']({
            message: err.response.data.error,
            description: ''
          })
        })
        .finally(() => {
          this.getArtifactDispatchConfig()
        })
    },
    metadataHandlerDelete(data) {
      globalSettingDeleteMetadata(data)
        .then(res => {
          this.successMsg(this.$t('Setting.DeletingMetadataWasSuccessful'))
        })
        .finally(() => {
          this.getMetadataConfiguration()
        })
    },
    getArtifactDispatchConfig() {
      getArtifactDispatchConfig().then(res => {
        this.artifactDispatchList = res
      })
    },
    artifactDispatchHandler(type, item) {
      this.artifactDispatchFormRest()
      if (item) {
        let data = Object.assign({}, item)
        this.artifactDispatchForm = data
      }
      this.handlerArtifactDispatchType = type
      this.showArtifactDispatchHandler = true
    },

    metadataHandlerCancel() {
      this.metadataFormReset()
      this.showMetadataHandler = false
    },
    artifactDispatchHandlerCancel() {
      this.artifactDispatchFormRest()
      this.showArtifactDispatchHandler = false
    },
    packageNameModalCancel() {
      this.showPackageNameModal = false
    },
    packageNameModalShow() {
      this.showPackageNameModal = true
    },
    allowedOriginsChange() {
      let allowedOrigins = this.serverSettings.corsConfigurationForm
        .allowedOrigins
      if (
        allowedOrigins &&
        allowedOrigins.length === 1 &&
        allowedOrigins[0] === '*'
      ) {
        this.serverSettings.corsConfigurationForm.corsAllowAll = true
      } else {
        this.serverSettings.corsConfigurationForm.corsAllowAll = false
      }
    },
    corsAllowAllChange(val) {
      if (val) {
        this.serverSettings.corsConfigurationForm.allowedOrigins = ['*']
      }
    },
    /**
     * 编辑单点登录客户端
     */
    ssoEdit(val) {
      this.ssoActionName = this.$t('Setting.EditClient')
      this.$nextTick(() => {
        this.ssoform.setFieldsValue({ ...val }) // loadsh的pick方
      })
      this.ssoDialogShow = true

    },
    /**
     * 删除单点登录客户端
     */
    ssoDelete(val) {
      deleteClient({ clientId: val.clientId }).then(() => {
        this.$notification.success({
          message: this.$t('Setting.Tips'),
          description: this.$t('Setting.DeleteClientSuccess'),
        })
        this.getSsoList()
      }).catch((err) => {
        this.$notification["error"]({
          message: err.response.data.error,
        })
      })
    },
    /**
     * 新增单点登录客户端
     */
    ssoAdd() {
      this.ssoActionName = this.$t('Setting.AddNewClient')
      this.ssoObj = {
        clientId: '',
        clientSecret: '',
        redirectPath: '',
        ssoPath: '',
        desc: '',
        picIcoin: '',
        loginOutRedPath: '',
        loginOutUrl: '',
        clientName: '',
        accessTokenUrl: '',
        userInfoUrl: '',
        username: '',
      }
      this.$nextTick(() => {
        this.ssoform.setFieldsValue({ ...this.ssoObj })
        this.ssoDialogShow = true
      })

    },
    /**
     * 提交客户端信息
     */
    handleOk() {

      // 校验
      this.ssoform.validateFields((err, fieldsValue) => {

        if (!err) {
          // 新增逻辑
          if (this.ssoActionName.includes(this.$t('Setting.Add'))) {
            addSsoClient(fieldsValue).then(() => {
              this.$notification.success({
                message: this.$t('Setting.Tips'),
                description: this.$t('Setting.AddedClientSuccess'),
              })
              this.ssoDialogShow = false
              this.getSsoList()
            }).catch((err) => {
              this.$notification["error"]({
                message: err.response.data.error,
              })
            })
          } else {
            updateSsoClient(fieldsValue).then(() => {
              this.$notification.success({
                message: this.$t('Setting.Tips'),
                description: this.$t('Setting.UpdateClientSuccess'),
              })
              this.ssoDialogShow = false
              this.getSsoList()
            }).catch((err) => {
              this.$notification["error"]({
                message: err.response.data.error,
              })
            })
          }
        }
      })
    },
    /**
     * 关闭对话框
     */
    handleCancel() {
      this.ssoDialogShow = false
    },
    packageNameRefresh() {
      this.$refs.packageName.getPackageNameList()
    },

    uploadLicense(info) {
      console.log("开始上传文件,info:", info);
      const formData = new FormData()
      formData.append("file", info.file)
      uploadLicenseFile(formData).then(() => {
        this.successMsg("上传成功");
        this.getMachineCode;
      }).catch(err => {
          this.$notification['error']({
            message: err.response.data.error,
        })
      });
    },  
  }
}
</script>

<style lang="scss" scoped>
::v-deep .ant-steps-item-process>.ant-steps-item-container>.ant-steps-item-content>.ant-steps-item-title {
  width: 200px;
}

#settings::v-deep {
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

    .ant-anchor-link-title-active {
      background-color: #eeeeee;
    }
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
    border-left: 1px solid hsla(0, 0%, 100%, 0.3);
    padding-left: 5px;
    padding-top: 2px;
    opacity: 0.75;
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
      border-left: 1px solid hsla(0, 0%, 100%, 0.3);
      padding-left: 5px;
      padding-top: 2px;
      opacity: 0.75;

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

  .white-group,
  .black-group {
    width: 100%;
    display: inline-flex;
    justify-content: flex-start;
    flex-wrap: wrap;
  }

  .white-group .white,
  .black-group .black {
    margin-right: 10px;
    margin-bottom: 10px;
    width: calc((100% - 50px) / 5);
  }

  // .white-card,.black-card{
  //   height:100%;
  //   margin-right: 10px;
  //   width: calc((100% - 20px) / 2);
  //   overflow-y: auto;
  // }
  .white-group .uuid,
  .black-group .uuid {
    font-size: 5px;
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

  .o-btn {
    width: 36px;
    height: 36px;
    margin-right: 8px;
    background-color: #1890ff;
    border-radius: 8px;
    display: inline-flex;
    justify-content: center;
    align-items: center;
  }

  .o-btn img {
    width: 20px;
    height: 20px;
    cursor: pointer;
  }

  .o-black {
    background-color: #f58080;
  }

  .ant-pagination-prev .ant-pagination-item-link,
  .ant-pagination-next .ant-pagination-item-link {
    margin-top: 2.5px;
  }

  .ant-pagination-item {
    margin-top: 5px;
  }

  .block-full {
    padding: 0 0 0 25px;
  }

  .block-form .block-full .ant-form-item-label,
  .block-form .block-full .ant-checkbox-group,
  .block-form .block-full .ant-form-item-control {
    line-height: 36px;
  }

  .block-form .block-full .ant-form-item {
    margin-bottom: unset;
  }

  .block-form .tips {
    color: grey;
    font-size: 12px;
    opacity: 0.7;
  }

  .cursor-pointer {
    cursor: pointer;
  }

  .package-name-add {
    vertical-align: middle;
  }

  .block-settings,
  .package-name-list.ant-card {
    box-shadow: unset;
  }

  .block-settings,
  .package-name-list.ant-card-bordered {
    border: unset;
  }
}

.info-message {
  color: #fff;
  background: #1890ff;
  border-radius: 50%;
  scale: (1.3);
}

/deep/ .ant-card-extra {
  display: flex;
}
</style>
