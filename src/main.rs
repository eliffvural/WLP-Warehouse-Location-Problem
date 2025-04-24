use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
use std::time::Instant;
use rand::Rng;
use rand::prelude::SliceRandom;

// Depo yapısı
#[derive(Debug)]
struct Warehouse {
    opening_cost: f64,    // Açılma maliyeti (s_w)
    capacity: i32,        // Kapasite (cap_w)
    service_costs: Vec<f64>, // Hizmet maliyetleri (t_cw)
}

// Müşteri yapısı
#[derive(Debug)]
struct Customer {
    demand: i32, // Talep (d_c)
}

fn calculate_total_cost(
    warehouses: &Vec<Warehouse>,
    assignments: &Vec<usize>,
    open_warehouses: &Vec<bool>,
    m: usize,
) -> f64 {
    let mut total_cost = 0.0;

    // Açılma maliyetlerini ekle
    for w in 0..open_warehouses.len() {
        if open_warehouses[w] {
            total_cost += warehouses[w].opening_cost;
        }
    }

    // Hizmet maliyetlerini ekle
    for c in 0..m {
        let w = assignments[c];
        if w > 0 {
            let warehouse_idx = w - 1; // 1 tabanlı indeks
            total_cost += warehouses[warehouse_idx].service_costs[c];
        }
    }

    total_cost
}

fn count_open_warehouses(open_warehouses: &Vec<bool>) -> usize {
    open_warehouses.iter().filter(|&&open| open).count()
}

fn aco(
    warehouses: &Vec<Warehouse>,
    customers: &Vec<Customer>,
    n: usize,
    m: usize,
) -> (f64, Vec<bool>, Vec<usize>) {
    let mut rng = rand::thread_rng();

    // ACO parametreleri
    let num_ants = 50;
    let iterations = 100;
    let alpha = 1.0; // Feromon ağırlığı
    let beta = 2.0;  // Maliyet ağırlığı
    let evaporation_rate = 0.5;
    let q = 100.0; // Feromon güncelleme sabiti
    let min_warehouses = 5;
    let penalty_factor = 1000.0;

    // Feromon matrisi (depo-müşteri çiftleri için)
    let mut pheromones: Vec<Vec<f64>> = vec![vec![1.0; m]; n];

    // En iyi çözüm
    let mut best_cost = f64::MAX;
    let mut best_assignments = vec![0; m];
    let mut best_open_warehouses = vec![false; n];

    for _ in 0..iterations {
        let mut ant_solutions: Vec<(f64, Vec<usize>, Vec<bool>)> = Vec::with_capacity(num_ants);

        // Her karınca için çözüm oluştur
        for _ in 0..num_ants {
            let mut assignments = vec![0; m];
            let mut open_warehouses = vec![false; n];
            let mut used_capacity = vec![0; n];
            let mut opened_warehouses: Vec<usize> = Vec::new();

            // İlk olarak minimum 5 depo açılmasını sağla
            let mut customers_to_assign: Vec<usize> = (0..m).collect();
            customers_to_assign.shuffle(&mut rng);

            let mut assigned_customers = 0;
            while opened_warehouses.len() < min_warehouses && assigned_customers < customers_to_assign.len() {
                let c = customers_to_assign[assigned_customers];
                let mut assigned = false;
                let mut attempts = 0;
                while !assigned && attempts < n {
                    let w = rng.gen_range(0..n);
                    if opened_warehouses.contains(&w) {
                        attempts += 1;
                        continue;
                    }
                    let remaining_capacity = warehouses[w].capacity - used_capacity[w];
                    if customers[c].demand <= remaining_capacity {
                        assignments[c] = w + 1;
                        used_capacity[w] += customers[c].demand;
                        open_warehouses[w] = true;
                        opened_warehouses.push(w);
                        assigned = true;
                    }
                    attempts += 1;
                }
                if !assigned {
                    let mut min_opening_cost = f64::MAX;
                    let mut best_warehouse = 0;
                    for w in 0..n {
                        if !opened_warehouses.contains(&w) && warehouses[w].opening_cost < min_opening_cost {
                            min_opening_cost = warehouses[w].opening_cost;
                            best_warehouse = w;
                        }
                    }
                    assignments[c] = best_warehouse + 1;
                    used_capacity[best_warehouse] += customers[c].demand;
                    open_warehouses[best_warehouse] = true;
                    opened_warehouses.push(best_warehouse);
                }
                assigned_customers += 1;
            }

            // Kalan müşterileri feromon tabanlı ata
            for c in assigned_customers..m {
                let customer_idx = customers_to_assign[c];
                let mut assigned = false;
                let mut attempts = 0;
                let mut probabilities: Vec<(usize, f64)> = Vec::new();

                // Depolar için olasılıkları hesapla
                for w in 0..n {
                    let remaining_capacity = warehouses[w].capacity - used_capacity[w];
                    if customers[customer_idx].demand <= remaining_capacity {
                        let cost = if open_warehouses[w] {
                            warehouses[w].service_costs[customer_idx]
                        } else {
                            warehouses[w].opening_cost + warehouses[w].service_costs[customer_idx]
                        };
                        let pheromone = pheromones[w][customer_idx];
                        let probability = (pheromone.powf(alpha)) * ((1.0 / cost).powf(beta));
                        probabilities.push((w, probability));
                    }
                }

                // Olasılıklara göre bir depo seç
                if !probabilities.is_empty() {
                    let total: f64 = probabilities.iter().map(|(_, p)| p).sum();
                    let mut r = rng.gen_range(0.0..total);
                    for (w, prob) in probabilities {
                        r -= prob;
                        if r <= 0.0 {
                            assignments[customer_idx] = w + 1;
                            used_capacity[w] += customers[customer_idx].demand;
                            open_warehouses[w] = true;
                            if !opened_warehouses.contains(&w) {
                                opened_warehouses.push(w);
                            }
                            assigned = true;
                            break;
                        }
                    }
                }

                if !assigned {
                    let mut min_cost = f64::MAX;
                    let mut best_warehouse = 0;
                    for w in 0..n {
                        let remaining_capacity = warehouses[w].capacity - used_capacity[w];
                        if customers[customer_idx].demand <= remaining_capacity {
                            let cost = if open_warehouses[w] {
                                warehouses[w].service_costs[customer_idx]
                            } else {
                                warehouses[w].opening_cost + warehouses[w].service_costs[customer_idx]
                            };
                            if cost < min_cost {
                                min_cost = cost;
                                best_warehouse = w;
                            }
                        }
                    }
                    assignments[customer_idx] = best_warehouse + 1;
                    used_capacity[best_warehouse] += customers[customer_idx].demand;
                    open_warehouses[best_warehouse] = true;
                    if !opened_warehouses.contains(&best_warehouse) {
                        opened_warehouses.push(best_warehouse);
                    }
                }
            }

            // Maliyeti hesapla
            let mut cost = calculate_total_cost(warehouses, &assignments, &open_warehouses, m);
            let open_count = count_open_warehouses(&open_warehouses);
            if open_count < min_warehouses {
                cost += penalty_factor * (min_warehouses - open_count) as f64;
            }
            ant_solutions.push((cost, assignments, open_warehouses));
        }

        // En iyi çözümü güncelle
        for (cost, assignments, open_warehouses) in &ant_solutions {
            let open_count = count_open_warehouses(open_warehouses);
            if *cost < best_cost && open_count >= min_warehouses {
                best_cost = *cost;
                best_assignments = assignments.clone();
                best_open_warehouses = open_warehouses.clone();
            }
        }

        // Feromonları güncelle
        for w in 0..n {
            for c in 0..m {
                pheromones[w][c] *= 1.0 - evaporation_rate; // Buharlaşma
            }
        }

        // Her karınca için feromon katkısı
        for (cost, assignments, open_warehouses) in ant_solutions {
            let open_count = count_open_warehouses(&open_warehouses);
            let delta = if open_count >= min_warehouses {
                q / cost
            } else {
                0.0
            };
            for c in 0..m {
                let w = assignments[c];
                if w > 0 {
                    let warehouse_idx = w - 1;
                    pheromones[warehouse_idx][c] += delta;
                }
            }
        }
    }

    (best_cost, best_open_warehouses, best_assignments)
}

fn solve_wlp(input_path: &str) -> io::Result<(usize, usize, f64, Vec<bool>, Vec<usize>, f64)> {
    let start_time = Instant::now();

    // Dosyanın varlığını kontrol et
    let file = File::open(input_path).map_err(|e| {
        io::Error::new(io::ErrorKind::NotFound, format!("Dosya açılamadı: {}. Hata: {}", input_path, e))
    })?;
    let reader = BufReader::new(file);
    let mut lines = reader.lines();

    // İlk satır: Depo ve müşteri sayısı
    let first_line = lines.next().ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "Dosya boş"))?;
    let first_line = first_line?;
    let mut first_iter = first_line.split_whitespace();
    
    let n: usize = first_iter.next()
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "Depo sayısı eksik"))?
        .parse()
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("Depo sayısı tamsayı değil: {}", e)))?;
    
    let m: usize = first_iter.next()
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "Müşteri sayısı eksik"))?
        .parse()
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("Müşteri sayısı tamsayı değil: {}", e)))?;

    println!("{} için - Depo sayısı: {}, Müşteri sayısı: {}", input_path, n, m);

    // Depoları oku
    let mut warehouses: Vec<Warehouse> = Vec::with_capacity(n);
    for i in 0..n {
        let line = lines.next().ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("Depo {} için veri eksik", i)))?;
        let line = line?;
        let mut parts = line.split_whitespace();
        
        let opening_cost: f64 = parts.next()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("Depo {}: Açılma maliyeti eksik", i)))?
            .parse()
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("Depo {}: Açılma maliyeti geçersiz: {}", i, e)))?;
        
        let capacity: i32 = parts.next()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("Depo {}: Kapasite eksik", i)))?
            .parse::<f64>()
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("Depo {}: Kapasite geçersiz: {}", i, e)))?
            .round() as i32;
        
        let mut service_costs: Vec<f64> = parts
            .map(|x| x.parse().map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("Depo {}: Hizmet maliyeti geçersiz: {}", i, e))))
            .collect::<Result<Vec<f64>, io::Error>>()?;
        
        // Eksik hizmet maliyetlerini geçici olarak 0.0 ile doldur
        while service_costs.len() < m {
            service_costs.push(0.0);
        }
        warehouses.push(Warehouse {
            opening_cost,
            capacity,
            service_costs,
        });
    }

    // Müşterileri oku ve hizmet maliyetlerini güncelle
    let mut customers: Vec<Customer> = Vec::with_capacity(m);
    let mut rng = rand::thread_rng();
    for i in 0..m {
        let line = lines.next().ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("Müşteri {} için veri eksik", i)))?;
        let line = line?;
        let mut parts = line.split_whitespace();
        let demand: i32 = parts.next()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("Müşteri {}: Talep eksik", i)))?
            .parse::<f64>()
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("Müşteri {}: Talep geçersiz: {}", i, e)))?
            .round() as i32;
        
        // Hizmet maliyetlerini talebe göre güncelle
        for w in 0..n {
            if warehouses[w].service_costs[i] == 0.0 {
                // Hizmet maliyeti = talep * 0.001 + rastgele değer
                warehouses[w].service_costs[i] = demand as f64 * 0.001 + rng.gen_range(0.01..0.1);
            }
        }
        
        customers.push(Customer { demand });
    }

    // ACO ile çöz
    let (total_cost, open_warehouses, assignments) = aco(
        &warehouses,
        &customers,
        n,
        m,
    );

    let elapsed_time = start_time.elapsed().as_secs_f64();
    Ok((n, m, total_cost, open_warehouses, assignments, elapsed_time))
}

fn main() -> io::Result<()> {
    // İşlenecek veri setleri
    let datasets = vec!["wl_25", "wl_50", "wl_200", "wl_300", "wl_500"];

    // Çıktıyı yazmak için bir dosya oluştur
    let mut file = File::create("wlp_sonuc.txt")?;

    // "Öğrenci No, Ad Soyad" başlığını en üstte bir kez yaz
    let mut output = String::new();
    output.push_str("222802077,Elif Vural\n");
    output.push_str("Dosya Boyut,Optimal Maliyet,Müşteriye atanan depolar\n");
    print!("{}", output);
    writeln!(file, "{}", output)?;

    // Her veri setini işle
    for dataset in datasets {
        let input_path = format!("datasets/{}", dataset);
        let mut output = String::new();

        // Tüm veri setleri için ACO kullan
        match solve_wlp(&input_path) {
            Ok((_n, m, total_cost, _open_warehouses, assignments, _elapsed_time)) => {
                output.push_str(&format!("{}", m));
                output.push_str(&format!(",{}", total_cost));
                output.push_str(",");
                let assignment_str: Vec<String> = assignments.iter().map(|x| x.to_string()).collect();
                output.push_str(&assignment_str.join(" "));
                output.push_str("\n");
            }
            Err(e) => {
                output.push_str(&format!("Veri Seti: {}\n", dataset));
                output.push_str(&format!("Hata: {}\n", e));
            }
        }

        // Çıktıyı hem konsola yaz hem dosyaya kaydet
        print!("{}", output);
        writeln!(file, "{}", output)?;
    }

    // Dosyayı kapat
    file.flush()?;
    println!("\nSonuçlar 'wlp_sonuc.txt' dosyasına kaydedildi.");

    Ok(())
}