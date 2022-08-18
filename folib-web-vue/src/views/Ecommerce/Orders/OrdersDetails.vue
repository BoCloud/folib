<!-- 
	This is the Order details page, it uses the dashboard layout in: 
	"./layouts/Dashboard.vue" .
 -->

<template>

	<div>

		<!-- Order Details card -->
		<a-card :bordered="false" class="card-order header-solid mb-24 mx-auto mt-20 mb-50" :bodyStyle="{paddingTop: 0}" style="max-width: 800px;">
			<template #title>
				<h6 class="mb-0">Order Details</h6>
			</template>

			<!-- Order Invoice row -->
			<a-row :gutter="[24]" type="flex">
				<a-col :span="24" :md="8">
					<p class="mb-0">
						Order no. <strong>241342</strong> from <strong>23.02.2021</strong>
					</p>
					<p class="mb-0">
						Code: <strong>KF332</strong>
					</p>
				</a-col>
				<a-col :span="24" :md="8" class="ml-auto text-right">
					<a-button class="px-20">INVOICE</a-button>
				</a-col>
			</a-row>
			<!-- / Order Invoice row -->

			<hr class="gradient-line">

			<!-- Order Products row -->
			<a-row :gutter="[24]" type="flex" class="order-products" align="middle">
				<a-col :span="24" :md="12">
					<div class="d-flex">
						<a-avatar class="mr-15" src="https://images.unsplash.com/photo-1511499767150-a48a237f0083?ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&ixlib=rb-1.2.1&auto=format&fit=crop&w=1400&q=80"
							shape="square" :size="110" />
						<div>
							<h6 class="mb-0 mt-10 font-semibold">Gold Glasses</h6>
							<p class="mb-15">Order was delivered 2 days ago.</p>
							<a-tag class="ant-tag-success font-semibold">DELIVERED</a-tag>
						</div>
					</div>
				</a-col>
				<a-col :span="24" :md="12" class="ml-auto text-right">
					<a-button type="primary" class="px-20 mb-10">CONTACT US</a-button>
					<p>Do you like the product? Leave us a review here.</p>
				</a-col>
			</a-row>
			<!-- / Order Products row -->

			<hr class="gradient-line">

			<a-row :gutter="[24]" type="flex">

				<!-- Track order timeline -->
				<a-col :span="24" :md="12" :lg="6">
					<h6 class="mb-15">Track order</h6>
					<a-timeline>
						<a-timeline-item color="green">
							Order received
							<small>22 DEC 7:20 AM</small>
						</a-timeline-item>
						<a-timeline-item color="green">
							Generate order id #1832412
							<small>22 DEC 7:21 AM</small>
						</a-timeline-item>
						<a-timeline-item color="blue">
							Order transmited to courier
							<small>22 DEC 8:10 AM</small>
						</a-timeline-item>
						<a-timeline-item color="blue">
							Order delivered
							<small>22 DEC 4:54 PM</small>
						</a-timeline-item>
					</a-timeline>
				</a-col>
				<!-- / Track order timeline -->
				
				<!-- Payment and billing info -->
				<a-col :span="24" :md="12" :lg="10">

					<!-- Payment Details -->
					<h6 class="mb-15">Payment Details</h6>
					<a-card class="payment-method-card mb-24">
						<img src="images/logos/mastercard-logo.png" alt="">
						<h6 class="card-number">**** **** **** 7362</h6>
						<a-tooltip>
							<template slot="title">We do not store card details</template>
							<a-button shape="circle" size="small" class="m-0">
								<a-icon class="m-0 text-dark" type="info" style="font-size: 10px;" />
							</a-button>
						</a-tooltip>
					</a-card>
					<!-- / Payment Details -->

					<!-- Billing Information -->
					<h6 class="mb-15">Billing Information</h6>
					<a-card :bordered="false" class="card-billing-info">
						<div class="col-info">
							<a-descriptions title="Oliver Liam" :column="1">
								<a-descriptions-item label="Company Name">
									Viking Burrito
								</a-descriptions-item>
								<a-descriptions-item label="Email Address">
									oliver@burrito.com
								</a-descriptions-item>
								<a-descriptions-item label="VAT Number">
									FRB1235476
								</a-descriptions-item>
							</a-descriptions>
						</div>
					</a-card>
					<!-- / Billing Information -->
					
				</a-col>
				<!-- / Payment and billing info -->
				
				<!-- Order Summary -->
				<a-col :span="24" :lg="6" class="ml-auto">
					<h6 class="mb-15">Order Summary</h6>
					<div class="d-flex mb-10">
						<span class="text-muted">
							Product Price:
						</span>
						<span class="text-lg text-dark font-semibold ml-auto">
							$90
						</span>
                  	</div>
					<div class="d-flex mb-10">
						<span class="text-muted">
							Delivery:
						</span>
						<span class="text-lg text-dark font-semibold ml-auto">
							$15
						</span>
                  	</div>
					<div class="d-flex mb-24">
						<span class="text-muted">
							Taxes:
						</span>
						<span class="text-lg text-dark font-semibold ml-auto">
							$1.95
						</span>
                  	</div>
					<div class="d-flex">
						<h5 class="font-regular text-muted">
							Total:
						</h5>
						<h5 class="text-dark font-semibold ml-auto">$105.95</h5>
					</div>
				</a-col>
				<!-- / Order Summary -->

			</a-row>

		</a-card>
		<!-- / Order Details card -->

	</div>

</template>

<script>

	export default {
		data() {
			return {
			};
		},
		methods: {

			// Print order details.
			print() {
				window.print() ;
			},
			
		},
	};
</script>

<style lang="scss" scoped>
	.ant-timeline-item {
		padding: 0 0 15px;
	}
</style>