pub mod mem2reg;
pub mod phi_eliminate;

pub use mem2reg::promote_allocas;
pub use mem2reg::promote_allocas_with_params;
pub use phi_eliminate::eliminate_phis;
